# 04_model_training.R
# Benchmark principal : deux strategies de desequilibre (none, smote) et
# modeles obligatoires. CatBoost est conserve dans la structure ; s'il n'est
# pas disponible dans l'environnement R, il est trace comme non execute.

required_packages <- c("tidymodels", "dplyr", "readr", "ggplot2", "here", "rpart", "ranger", "xgboost")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Packages manquants : ", paste(missing_packages, collapse = ", "),
    "\nInstallez-les par exemple avec : install.packages(c('",
    paste(missing_packages, collapse = "','"), "'))"
  )
}

library(tidymodels)
library(dplyr)
library(readr)
library(ggplot2)
library(here)

set.seed(123)
options(yardstick.event_first = FALSE)

dir.create(here("models"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("reports"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("reports", "figures"), recursive = TRUE, showWarnings = FALSE)

train_path <- here("data", "processed", "train_data.rds")
test_path <- here("data", "processed", "test_data.rds")
clean_path <- here("data", "processed", "data_clean.rds")
recipe_path <- here("data", "processed", "recipe_base.rds")

if (!file.exists(train_path) || !file.exists(test_path) || !file.exists(recipe_path)) {
  stop("Executez d'abord scripts/03_preprocessing.R.")
}

train_data <- readRDS(train_path)
test_data <- readRDS(test_path)
data_clean <- if (file.exists(clean_path)) readRDS(clean_path) else NULL
recipe_base <- readRDS(recipe_path)

if (!"claim_status" %in% names(train_data)) {
  stop("Variable cible introuvable : claim_status")
}

if (!setequal(levels(train_data$claim_status), c("no", "yes"))) {
  stop("La cible doit avoir les niveaux no / yes.")
}

smote_available <- requireNamespace("themis", quietly = TRUE)
catboost_available <- requireNamespace("catboost", quietly = TRUE)

recipe_smote <- recipe_base
if (smote_available) {
  # SMOTE est ajoute a la recette : il sera appris uniquement sur les donnees
  # d'entrainement de chaque fold, puis ignore lors de la prediction test.
  recipe_smote <- recipe_base %>%
    themis::step_smote(claim_status, over_ratio = 0.30, skip = TRUE)
} else {
  message("Package themis absent : la strategie SMOTE sera tracee comme non executee.")
}

class_balance <- function(data, dataset_stage, balancing_strategy) {
  counts <- data %>%
    count(claim_status, name = "n") %>%
    tidyr::complete(claim_status = factor(c("no", "yes"), levels = c("no", "yes")), fill = list(n = 0))

  n_no <- counts %>% filter(claim_status == "no") %>% pull(n)
  n_yes <- counts %>% filter(claim_status == "yes") %>% pull(n)
  total <- n_no + n_yes

  tibble(
    dataset_stage = dataset_stage,
    balancing_strategy = balancing_strategy,
    n_no = n_no,
    n_yes = n_yes,
    prop_no = n_no / total,
    prop_yes = n_yes / total,
    total_observations = total
  )
}

class_balance_rows <- list(
  if (!is.null(data_clean)) {
    data_clean %>%
      mutate(claim_status = factor(claim_status, levels = c("no", "yes"))) %>%
      class_balance("full_data_original", "none")
  },
  class_balance(train_data, "train_before_smote", "none"),
  class_balance(test_data, "test_unchanged", "none")
)

if (smote_available) {
  # Cette preparation illustre l'effet de SMOTE sur un echantillon
  # d'entrainement. Elle n'est jamais appliquee au test set.
  train_after_smote <- recipe_smote %>%
    prep(training = train_data, verbose = FALSE) %>%
    bake(new_data = NULL)

  class_balance_rows <- append(
    class_balance_rows,
    list(class_balance(train_after_smote, "train_after_smote", "smote"))
  )
}

class_balance_summary <- bind_rows(class_balance_rows) %>%
  mutate(
    dataset_stage = factor(
      dataset_stage,
      levels = c("full_data_original", "train_before_smote", "train_after_smote", "test_unchanged")
    )
  ) %>%
  arrange(dataset_stage) %>%
  mutate(dataset_stage = as.character(dataset_stage))

write_csv(class_balance_summary, here("reports", "class_balance_summary.csv"))

print(class_balance_summary)

p_balance <- class_balance_summary %>%
  filter(dataset_stage %in% c("train_before_smote", "train_after_smote", "test_unchanged")) %>%
  select(dataset_stage, prop_no, prop_yes) %>%
  tidyr::pivot_longer(cols = c(prop_no, prop_yes), names_to = "class", values_to = "proportion") %>%
  mutate(class = recode(class, prop_no = "no", prop_yes = "yes")) %>%
  ggplot(aes(x = dataset_stage, y = proportion, fill = class)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_manual(values = c(no = "#2b6cb0", yes = "#c53030")) +
  labs(
    title = "Effet de SMOTE sur le déséquilibre de classes",
    x = NULL,
    y = "Proportion",
    fill = "Classe"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

ggsave(here("reports", "figures", "class_balance_smote_effect.png"), p_balance, width = 8, height = 5)

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

strategy_recipes <- list(none = recipe_base)
if (smote_available) {
  strategy_recipes$smote <- recipe_smote
}

folds <- vfold_cv(train_data, v = 5, strata = claim_status)

# On sauvegarde les predictions de validation croisee et on calcule ensuite
# les metriques explicitement sur .pred_yes. Cela evite toute ambiguite de
# yardstick sur la colonne de probabilite associee a l'evenement positif.
resample_metrics <- metric_set(accuracy, roc_auc)
control <- control_resamples(save_pred = TRUE, verbose = FALSE)

compute_fold_metrics <- function(predictions) {
  threshold_grid <- seq(0.01, 0.50, by = 0.01)

  # Audit ML : le seuil 0,5 est peu pertinent pour un sinistre rare. Chaque
  # configuration est donc comparee avec un seuil calibre sur ses predictions
  # out-of-fold, sans toucher au test set.
  threshold_metrics <- purrr::map_dfr(threshold_grid, function(threshold) {
    pred_class <- factor(
      ifelse(predictions$.pred_yes >= threshold, "yes", "no"),
      levels = c("no", "yes")
    )

    tp <- sum(predictions$claim_status == "yes" & pred_class == "yes")
    fp <- sum(predictions$claim_status == "no" & pred_class == "yes")
    fn <- sum(predictions$claim_status == "yes" & pred_class == "no")
    tn <- sum(predictions$claim_status == "no" & pred_class == "no")

    precision_value <- ifelse(tp + fp > 0, tp / (tp + fp), NA_real_)
    recall_value <- ifelse(tp + fn > 0, tp / (tp + fn), NA_real_)
    f1_value <- ifelse(
      is.na(precision_value) || is.na(recall_value) || precision_value + recall_value == 0,
      NA_real_,
      2 * precision_value * recall_value / (precision_value + recall_value)
    )

    tibble(
      threshold = threshold,
      accuracy = (tp + tn) / (tp + fp + fn + tn),
      precision = precision_value,
      recall = recall_value,
      f_meas = f1_value
    )
  })

  best_threshold_row <- threshold_metrics %>%
    mutate(
      sort_f1 = ifelse(is.na(f_meas), -Inf, f_meas),
      sort_recall = ifelse(is.na(recall), -Inf, recall),
      sort_precision = ifelse(is.na(precision), -Inf, precision)
    ) %>%
    arrange(desc(sort_f1), desc(sort_recall), desc(sort_precision)) %>%
    slice(1) %>%
    select(-sort_f1, -sort_recall, -sort_precision)

  roc_auc_value <- roc_auc(predictions, truth = claim_status, .pred_yes, event_level = "second")$.estimate
  pr_auc_value <- pr_auc(predictions, truth = claim_status, .pred_yes, event_level = "second")$.estimate

  list(
    metrics = tibble(
      .metric = c("accuracy", "precision", "recall", "f_meas", "roc_auc", "pr_auc"),
      .estimator = "binary",
      mean = c(
        best_threshold_row$accuracy,
        best_threshold_row$precision,
        best_threshold_row$recall,
        best_threshold_row$f_meas,
        roc_auc_value,
        pr_auc_value
      ),
      n = 1L,
      std_err = NA_real_
    ),
    threshold = best_threshold_row,
    threshold_grid = threshold_metrics
  )
}

threshold_registry <- list()

fit_one_configuration <- function(model_name, strategy_name, model_spec, rec) {
  config_id <- paste(model_name, strategy_name, sep = "__")
  message("Entrainement CV : ", config_id)
  resample_path <- here("models", paste0(config_id, "_resamples.rds"))
  fit_path <- here("models", paste0(config_id, "_fit.rds"))

  wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(model_spec)

  result <- tryCatch(
    {
      cache_is_valid <- FALSE
      if (file.exists(resample_path) && file.exists(fit_path)) {
        cached_resamples <- readRDS(resample_path)
        cached_predictions <- tryCatch(collect_predictions(cached_resamples), error = function(e) NULL)
        cache_is_valid <- !is.null(cached_predictions) && ".pred_yes" %in% names(cached_predictions)
      }

      if (cache_is_valid) {
        message("  -> reutilisation des objets existants pour ", config_id)
        resamples <- cached_resamples
      } else {
        message("  -> recalcul des resamples avec probabilites pour ", config_id)
        resamples <- fit_resamples(
          wf,
          resamples = folds,
          metrics = resample_metrics,
          control = control
        )

        final_fit <- fit(wf, data = train_data)

        saveRDS(resamples, resample_path)
        saveRDS(final_fit, fit_path)
      }

      predictions <- collect_predictions(resamples)
      computed <- compute_fold_metrics(predictions)
      threshold_registry[[config_id]] <<- computed$threshold %>%
        mutate(config_id = config_id, model = model_name, strategy = strategy_name, .before = 1)

      computed$metrics %>%
        mutate(
          model = model_name,
          strategy = strategy_name,
          status = "trained",
          reason = NA_character_,
          config_id = config_id,
          .before = 1
        )
    },
    error = function(e) {
      tibble(
        config_id = config_id,
        model = model_name,
        strategy = strategy_name,
        status = "failed",
        reason = conditionMessage(e),
        .metric = c("accuracy", "precision", "recall", "f_meas", "roc_auc", "pr_auc"),
        .estimator = NA_character_,
        mean = NA_real_,
        n = NA_integer_,
        std_err = NA_real_
      )
    }
  )

  result
}

model_comparison <- purrr::imap_dfr(strategy_recipes, function(rec, strategy_name) {
  purrr::imap_dfr(model_specs, function(model_spec, model_name) {
    fit_one_configuration(model_name, strategy_name, model_spec, rec)
  })
})

model_thresholds <- bind_rows(threshold_registry)

# CatBoost : le package R n'est pas disponible sur CRAN pour l'environnement
# courant. On garde deux configurations explicites pour documenter la limite
# technique sans bloquer les autres modeles obligatoires.
catboost_rows <- tidyr::expand_grid(
  model = "catboost",
  strategy = c("none", "smote"),
  .metric = c("accuracy", "precision", "recall", "f_meas", "roc_auc", "pr_auc")
) %>%
  mutate(
    config_id = paste(model, strategy, sep = "__"),
    status = if (catboost_available) "failed" else "skipped",
    reason = if (catboost_available) {
      "CatBoost detecte mais aucun moteur tidymodels stable n'est configure dans ce pipeline."
    } else {
      "Package catboost indisponible pour cette version de R via CRAN ; configuration conservee mais non executee."
    },
    .estimator = NA_character_,
    mean = NA_real_,
    n = NA_integer_,
    std_err = NA_real_
  ) %>%
  select(config_id, model, strategy, status, reason, .metric, .estimator, mean, n, std_err)

model_comparison <- bind_rows(model_comparison, catboost_rows) %>%
  arrange(model, strategy, .metric)

model_ranking <- model_comparison %>%
  filter(status == "trained", .metric %in% c("roc_auc", "f_meas", "recall")) %>%
  select(config_id, model, strategy, .metric, mean) %>%
  tidyr::pivot_wider(names_from = .metric, values_from = mean)

for (metric_name in c("roc_auc", "f_meas", "recall")) {
  if (!metric_name %in% names(model_ranking)) {
    model_ranking[[metric_name]] <- NA_real_
  }
}

model_ranking <- model_ranking %>%
  mutate(
    sort_roc_auc = ifelse(is.na(roc_auc), -Inf, roc_auc),
    sort_f_meas = ifelse(is.na(f_meas), -Inf, f_meas),
    sort_recall = ifelse(is.na(recall), -Inf, recall)
  ) %>%
  arrange(desc(.data[["sort_roc_auc"]]), desc(.data[["sort_f_meas"]]), desc(.data[["sort_recall"]])) %>%
  select(-sort_roc_auc, -sort_f_meas, -sort_recall)

write_csv(model_comparison, here("reports", "model_comparison.csv"))
write_csv(model_ranking, here("reports", "model_ranking.csv"))
write_csv(model_thresholds, here("reports", "model_thresholds.csv"))

saveRDS(model_comparison, here("models", "model_comparison.rds"))
saveRDS(model_ranking, here("models", "model_ranking.rds"))
saveRDS(model_thresholds, here("models", "model_thresholds.rds"))

message("Benchmark termine : reports/model_comparison.csv et reports/model_ranking.csv crees.")
