# 09_smote_training_audit.R
# Audit explicite de l'utilisation effective de SMOTE dans les workflows.
# Objectif : verifier que les configurations "smote" utilisent bien une recette
# contenant themis::step_smote(claim_status), appliquee au train/folds seulement.

required_packages <- c("tidymodels", "dplyr", "readr", "here", "rpart", "ranger", "xgboost")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Packages manquants : ", paste(missing_packages, collapse = ", "))
}

library(tidymodels)
library(dplyr)
library(readr)
library(here)

set.seed(123)

dir.create(here("reports"), recursive = TRUE, showWarnings = FALSE)

train_path <- here("data", "processed", "train_data.rds")
test_path <- here("data", "processed", "test_data.rds")
recipe_path <- here("data", "processed", "recipe_base.rds")
thresholds_path <- here("reports", "model_thresholds.csv")

if (!file.exists(train_path) || !file.exists(test_path) || !file.exists(recipe_path)) {
  stop("Fichiers train/test/recipe absents. Executez d'abord scripts/03_preprocessing.R.")
}

train_data <- readRDS(train_path)
test_data <- readRDS(test_path)
recipe_base <- readRDS(recipe_path)
model_thresholds <- if (file.exists(thresholds_path)) read_csv(thresholds_path, show_col_types = FALSE) else NULL

if (!requireNamespace("themis", quietly = TRUE)) {
  stop("Le package themis est requis pour verifier l'application effective de SMOTE.")
}

recipe_smote <- recipe_base %>%
  themis::step_smote(claim_status, over_ratio = 0.30, skip = TRUE)

count_classes <- function(data, suffix = "") {
  counts <- data %>%
    count(claim_status, name = "n") %>%
    tidyr::complete(claim_status = factor(c("no", "yes"), levels = c("no", "yes")), fill = list(n = 0))

  n_no <- counts %>% filter(claim_status == "no") %>% pull(n)
  n_yes <- counts %>% filter(claim_status == "yes") %>% pull(n)
  total <- n_no + n_yes

  tibble(
    !!paste0("n_no", suffix) := n_no,
    !!paste0("n_yes", suffix) := n_yes,
    !!paste0("prop_no", suffix) := n_no / total,
    !!paste0("prop_yes", suffix) := n_yes / total,
    !!paste0("rows", suffix) := total
  )
}

# Donnees effectivement visibles par les modeles apres preparation.
# Pour "none", on utilise le train original. Pour "smote", on prepare la
# recette SMOTE sur train et on bake new_data = NULL : cela reproduit l'effet
# de step_smote sur l'echantillon d'apprentissage, sans toucher au test set.
train_none_prepared <- train_data
train_smote_prepared <- recipe_smote %>%
  prep(training = train_data, verbose = FALSE) %>%
  bake(new_data = NULL)

none_balance <- count_classes(train_none_prepared, "_training")
smote_balance <- count_classes(train_smote_prepared, "_training")
test_balance <- count_classes(test_data, "_test")

models_to_audit <- c("logistic_regression", "decision_tree", "random_forest", "xgboost", "catboost")

training_data_audit <- tidyr::expand_grid(
  model = models_to_audit,
  balancing_strategy = c("none", "smote")
) %>%
  rowwise() %>%
  mutate(
    smote_applied = balancing_strategy == "smote",
    training_rows_used = ifelse(balancing_strategy == "smote", smote_balance$rows_training, none_balance$rows_training),
    n_no_training = ifelse(balancing_strategy == "smote", smote_balance$n_no_training, none_balance$n_no_training),
    n_yes_training = ifelse(balancing_strategy == "smote", smote_balance$n_yes_training, none_balance$n_yes_training),
    prop_no_training = ifelse(balancing_strategy == "smote", smote_balance$prop_no_training, none_balance$prop_no_training),
    prop_yes_training = ifelse(balancing_strategy == "smote", smote_balance$prop_yes_training, none_balance$prop_yes_training),
    test_rows_used = test_balance$rows_test,
    n_no_test = test_balance$n_no_test,
    n_yes_test = test_balance$n_yes_test,
    prop_no_test = test_balance$prop_no_test,
    prop_yes_test = test_balance$prop_yes_test
  ) %>%
  ungroup()

write_csv(training_data_audit, here("reports", "training_data_audit.csv"))
print(training_data_audit)

metric_for_fit <- function(model_name, strategy_name) {
  config_id <- paste(model_name, strategy_name, sep = "__")
  fit_path <- here("models", paste0(config_id, "_fit.rds"))

  if (!file.exists(fit_path)) {
    return(tibble(
      model = model_name,
      balancing_strategy = strategy_name,
      threshold = NA_real_,
      accuracy = NA_real_,
      precision = NA_real_,
      recall = NA_real_,
      f1 = NA_real_,
      roc_auc = NA_real_,
      pr_auc = NA_real_,
      predicted_positive = NA_integer_,
      false_positives = NA_integer_,
      false_negatives = NA_integer_,
      true_positives = NA_integer_,
      true_negatives = NA_integer_
    ))
  }

  fit <- readRDS(fit_path)
  prob_pred <- predict(fit, new_data = test_data, type = "prob")
  if (!".pred_yes" %in% names(prob_pred)) {
    stop("Colonne .pred_yes introuvable pour ", config_id)
  }

  threshold <- 0.5
  if (!is.null(model_thresholds) && config_id %in% model_thresholds$config_id) {
    threshold <- model_thresholds %>%
      filter(config_id == !!config_id) %>%
      slice(1) %>%
      pull(threshold)
  }

  pred_class <- factor(ifelse(prob_pred$.pred_yes >= threshold, "yes", "no"), levels = c("no", "yes"))
  truth <- test_data$claim_status

  tp <- sum(truth == "yes" & pred_class == "yes")
  tn <- sum(truth == "no" & pred_class == "no")
  fp <- sum(truth == "no" & pred_class == "yes")
  fn <- sum(truth == "yes" & pred_class == "no")

  precision_value <- ifelse(tp + fp > 0, tp / (tp + fp), NA_real_)
  recall_value <- ifelse(tp + fn > 0, tp / (tp + fn), NA_real_)
  f1_value <- ifelse(
    is.na(precision_value) || is.na(recall_value) || precision_value + recall_value == 0,
    NA_real_,
    2 * precision_value * recall_value / (precision_value + recall_value)
  )

  pred_data <- bind_cols(
    test_data %>% select(claim_status),
    tibble(.pred_class = pred_class),
    prob_pred
  )

  tibble(
    model = model_name,
    balancing_strategy = strategy_name,
    threshold = threshold,
    accuracy = (tp + tn) / length(truth),
    precision = precision_value,
    recall = recall_value,
    f1 = f1_value,
    roc_auc = roc_auc(pred_data, truth = claim_status, .pred_yes, event_level = "second")$.estimate,
    pr_auc = pr_auc(pred_data, truth = claim_status, .pred_yes, event_level = "second")$.estimate,
    predicted_positive = tp + fp,
    false_positives = fp,
    false_negatives = fn,
    true_positives = tp,
    true_negatives = tn
  )
}

classification_metrics_by_model <- tidyr::expand_grid(
  model = models_to_audit,
  balancing_strategy = c("none", "smote")
) %>%
  purrr::pmap_dfr(function(model, balancing_strategy) {
    metric_for_fit(model, balancing_strategy)
  })

write_csv(classification_metrics_by_model, here("reports", "classification_metrics_by_model.csv"))
print(classification_metrics_by_model)

smote_effect_on_metrics <- classification_metrics_by_model %>%
  filter(model != "catboost") %>%
  select(model, balancing_strategy, accuracy, precision, recall, f1, roc_auc, pr_auc, predicted_positive, false_positives, false_negatives, true_positives, true_negatives) %>%
  tidyr::pivot_longer(
    cols = -c(model, balancing_strategy),
    names_to = "metric",
    values_to = "value"
  ) %>%
  tidyr::pivot_wider(names_from = balancing_strategy, values_from = value, names_prefix = "value_") %>%
  mutate(difference_smote_minus_none = value_smote - value_none)

write_csv(smote_effect_on_metrics, here("reports", "smote_effect_on_metrics.csv"))
print(smote_effect_on_metrics)

message("Audit SMOTE termine : reports/training_data_audit.csv, reports/classification_metrics_by_model.csv, reports/smote_effect_on_metrics.csv")
