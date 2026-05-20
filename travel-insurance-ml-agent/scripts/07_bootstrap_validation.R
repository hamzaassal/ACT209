# 07_bootstrap_validation.R
# Validation bootstrap du modele champion.
# Le bootstrap sert a mesurer la stabilite des performances. Il ne corrige pas
# le desequilibre de classes et n'est applique qu'au modele champion.

required_packages <- c("tidymodels", "dplyr", "readr", "ggplot2", "here", "rpart", "ranger", "xgboost")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Packages manquants : ", paste(missing_packages, collapse = ", "))
}

library(tidymodels)
library(dplyr)
library(readr)
library(ggplot2)
library(here)

set.seed(123)
options(yardstick.event_first = FALSE)

dir.create(here("reports"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("reports", "figures"), recursive = TRUE, showWarnings = FALSE)

B <- as.integer(Sys.getenv("BOOTSTRAP_B", unset = "100"))
if (is.na(B) || B <= 0) {
  stop("BOOTSTRAP_B doit etre un entier positif.")
}

ranking_path <- here("reports", "model_ranking.csv")
train_path <- here("data", "processed", "train_data.rds")
test_path <- here("data", "processed", "test_data.rds")
recipe_path <- here("data", "processed", "recipe_base.rds")
threshold_path <- here("models", "decision_threshold.rds")

if (!file.exists(ranking_path) || !file.exists(train_path) || !file.exists(test_path) || !file.exists(recipe_path)) {
  stop("Fichiers requis absents. Executez les scripts 03, 04, 05 et 06 avant le bootstrap.")
}

model_ranking <- read_csv(ranking_path, show_col_types = FALSE)
train_data <- readRDS(train_path)
test_data <- readRDS(test_path)
recipe_base <- readRDS(recipe_path)
decision_threshold <- if (file.exists(threshold_path)) readRDS(threshold_path)$threshold else 0.5

champion <- model_ranking %>% arrange(desc(roc_auc), desc(f_meas), desc(recall)) %>% slice(1)
champion_model <- champion$model
champion_strategy <- champion$strategy

prepped_recipe <- prep(recipe_base, training = train_data, verbose = FALSE)
n_predictors <- bake(prepped_recipe, new_data = train_data[0, ]) %>%
  select(-claim_status) %>%
  ncol()
rf_mtry <- max(1, floor(sqrt(n_predictors)))

model_specs <- list(
  logistic_regression = logistic_reg() %>%
    set_engine("glm") %>%
    set_mode("classification"),
  decision_tree = decision_tree(cost_complexity = 0.01, tree_depth = 6, min_n = 20) %>%
    set_engine("rpart") %>%
    set_mode("classification"),
  random_forest = rand_forest(trees = 60, mtry = rf_mtry, min_n = 15) %>%
    set_engine("ranger", importance = "impurity", probability = TRUE, num.threads = 2) %>%
    set_mode("classification"),
  xgboost = boost_tree(trees = 100, tree_depth = 4, learn_rate = 0.05, loss_reduction = 0, min_n = 10) %>%
    set_engine("xgboost", nthread = 2, verbosity = 0) %>%
    set_mode("classification")
)

if (!champion_model %in% names(model_specs)) {
  stop("Bootstrap impossible : modele champion non pris en charge par ce script : ", champion_model)
}

rec <- recipe_base
if (champion_strategy == "smote") {
  if (!requireNamespace("themis", quietly = TRUE)) {
    stop("Le champion utilise SMOTE mais le package themis est absent.")
  }
  rec <- recipe_base %>% themis::step_smote(claim_status, over_ratio = 0.30, skip = TRUE)
}

champion_workflow <- workflow() %>%
  add_recipe(rec) %>%
  add_model(model_specs[[champion_model]])

metric_one <- function(pred_data) {
  bind_rows(
    accuracy(pred_data, truth = claim_status, estimate = .pred_class),
    precision(pred_data, truth = claim_status, estimate = .pred_class, event_level = "second"),
    recall(pred_data, truth = claim_status, estimate = .pred_class, event_level = "second"),
    f_meas(pred_data, truth = claim_status, estimate = .pred_class, event_level = "second"),
    roc_auc(pred_data, truth = claim_status, .pred_yes, event_level = "second"),
    pr_auc(pred_data, truth = claim_status, .pred_yes, event_level = "second")
  )
}

bootstrap_results <- purrr::map_dfr(seq_len(B), function(b) {
  message("Bootstrap ", b, "/", B)
  boot_index <- sample(seq_len(nrow(train_data)), size = nrow(train_data), replace = TRUE)
  boot_train <- train_data[boot_index, , drop = FALSE]

  tryCatch(
    {
      fit_b <- fit(champion_workflow, data = boot_train)
      prob_b <- predict(fit_b, new_data = test_data, type = "prob")
      pred_data <- bind_cols(
        test_data %>% select(claim_status),
        tibble(.pred_class = factor(ifelse(prob_b$.pred_yes >= decision_threshold, "yes", "no"), levels = c("no", "yes"))),
        prob_b
      )

      metric_one(pred_data) %>%
        transmute(replication = b, metric = .metric, value = .estimate)
    },
    error = function(e) {
      tibble(
        replication = b,
        metric = c("accuracy", "precision", "recall", "f_meas", "roc_auc", "pr_auc"),
        value = NA_real_
      )
    }
  )
})

bootstrap_summary <- bootstrap_results %>%
  group_by(metric) %>%
  summarise(
    n_success = sum(!is.na(value)),
    mean = mean(value, na.rm = TRUE),
    sd = sd(value, na.rm = TRUE),
    q025 = quantile(value, 0.025, na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    q975 = quantile(value, 0.975, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    config_id = champion$config_id,
    model = champion$model,
    strategy = champion$strategy,
    B = B,
    .before = 1
  )

write_csv(bootstrap_results, here("reports", "bootstrap_results.csv"))
write_csv(bootstrap_summary, here("reports", "bootstrap_summary.csv"))

plot_boot_metric <- function(metric_name, file_name, title) {
  p <- bootstrap_results %>%
    filter(metric == metric_name) %>%
    ggplot(aes(x = value)) +
    geom_histogram(bins = 25, fill = "#2b6cb0", color = "white") +
    labs(title = title, x = metric_name, y = "Nombre de repetitions") +
    theme_minimal()

  ggsave(here("reports", "figures", file_name), p, width = 7, height = 5)
}

plot_boot_metric("roc_auc", "bootstrap_auc_distribution.png", "Distribution bootstrap de la ROC AUC")
plot_boot_metric("f_meas", "bootstrap_f1_distribution.png", "Distribution bootstrap du F1-score")
plot_boot_metric("recall", "bootstrap_recall_distribution.png", "Distribution bootstrap du recall")

print(bootstrap_summary)
message("Validation bootstrap terminee.")
