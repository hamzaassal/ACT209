# 11_smote_intensity_analysis.R
# Analyse complementaire de sensibilite a l'intensite de SMOTE.
# SMOTE est applique uniquement dans les folds de validation croisee et lors du
# fit final sur train. Le test set reste toujours dans sa distribution reelle.

required_packages <- c(
  "tidymodels", "dplyr", "readr", "here", "rpart", "ranger", "xgboost", "themis"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Packages manquants : ", paste(missing_packages, collapse = ", "),
    "\nInstallez-les avec install.packages() puis relancez le script."
  )
}

library(tidymodels)
library(dplyr)
library(readr)
library(here)

set.seed(123)
options(yardstick.event_first = FALSE)

dir.create(here("models"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("reports"), recursive = TRUE, showWarnings = FALSE)

train_path <- here("data", "processed", "train_data.rds")
test_path <- here("data", "processed", "test_data.rds")
recipe_path <- here("data", "processed", "recipe_base.rds")

if (!file.exists(train_path) || !file.exists(test_path) || !file.exists(recipe_path)) {
  stop("Fichiers train/test/recipe absents. Executez d'abord scripts/03_preprocessing.R.")
}

train_data <- readRDS(train_path)
test_data <- readRDS(test_path)
recipe_base <- readRDS(recipe_path)

if (!"claim_status" %in% names(train_data)) {
  stop("Variable cible introuvable : claim_status")
}

if (!setequal(levels(train_data$claim_status), c("no", "yes"))) {
  stop("La cible doit avoir les niveaux no / yes.")
}

count_balance <- function(data, balancing_strategy, over_ratio = NA_real_) {
  counts <- data %>%
    count(claim_status, name = "n") %>%
    tidyr::complete(
      claim_status = factor(c("no", "yes"), levels = c("no", "yes")),
      fill = list(n = 0)
    )

  n_no <- counts %>% filter(claim_status == "no") %>% pull(n)
  n_yes <- counts %>% filter(claim_status == "yes") %>% pull(n)
  total <- n_no + n_yes

  tibble(
    balancing_strategy = balancing_strategy,
    over_ratio = over_ratio,
    n_no = n_no,
    n_yes = n_yes,
    prop_no = n_no / total,
    prop_yes = n_yes / total,
    total_observations = total
  )
}

make_smote_recipe <- function(over_ratio) {
  recipe_base %>%
    themis::step_smote(claim_status, over_ratio = over_ratio, skip = TRUE)
}

strategy_recipes <- list(
  none = recipe_base,
  smote_10 = make_smote_recipe(0.111),
  smote_20 = make_smote_recipe(0.25)
)

strategy_over_ratios <- c(none = NA_real_, smote_10 = 0.111, smote_20 = 0.25)

smote_intensity_class_balance <- purrr::imap_dfr(strategy_recipes, function(rec, strategy_name) {
  prepared_train <- if (strategy_name == "none") {
    train_data
  } else {
    rec %>%
      prep(training = train_data, verbose = FALSE) %>%
      bake(new_data = NULL)
  }

  count_balance(
    data = prepared_train,
    balancing_strategy = strategy_name,
    over_ratio = strategy_over_ratios[[strategy_name]]
  )
}) %>%
  mutate(
    test_n_no = sum(test_data$claim_status == "no"),
    test_n_yes = sum(test_data$claim_status == "yes"),
    test_prop_yes = test_n_yes / nrow(test_data),
    test_total_observations = nrow(test_data)
  )

write_csv(smote_intensity_class_balance, here("reports", "smote_intensity_class_balance.csv"))
print(smote_intensity_class_balance)

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

folds <- vfold_cv(train_data, v = 5, strata = claim_status)
resample_metrics <- metric_set(accuracy, roc_auc)
control <- control_resamples(save_pred = TRUE, verbose = FALSE)

choose_threshold_from_oof <- function(predictions) {
  threshold_grid <- seq(0.01, 0.50, by = 0.01)

  purrr::map_dfr(threshold_grid, function(threshold) {
    pred_class <- factor(ifelse(predictions$.pred_yes >= threshold, "yes", "no"), levels = c("no", "yes"))

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
      f1_score = f1_value
    )
  }) %>%
    mutate(
      sort_f1 = ifelse(is.na(f1_score), -Inf, f1_score),
      sort_recall = ifelse(is.na(recall), -Inf, recall),
      sort_precision = ifelse(is.na(precision), -Inf, precision)
    ) %>%
    arrange(desc(sort_f1), desc(sort_recall), desc(sort_precision)) %>%
    slice(1) %>%
    select(-sort_f1, -sort_recall, -sort_precision)
}

evaluate_on_test <- function(fitted_workflow, threshold) {
  prob_pred <- predict(fitted_workflow, new_data = test_data, type = "prob")
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
    tibble(claim_status = truth, .pred_class = pred_class),
    prob_pred
  )

  tibble(
    threshold = threshold,
    accuracy = (tp + tn) / length(truth),
    precision = precision_value,
    recall = recall_value,
    f1_score = f1_value,
    roc_auc = roc_auc(pred_data, truth = claim_status, .pred_yes, event_level = "second")$.estimate,
    pr_auc = pr_auc(pred_data, truth = claim_status, .pred_yes, event_level = "second")$.estimate,
    predicted_positive = tp + fp,
    false_positives = fp,
    false_negatives = fn,
    true_positives = tp,
    true_negatives = tn
  )
}

fit_and_evaluate <- function(model_name, balancing_strategy, model_spec, rec) {
  config_id <- paste(model_name, balancing_strategy, sep = "__")
  message("Analyse intensite SMOTE : ", config_id)

  resample_path <- here("models", paste0("smote_intensity_", config_id, "_resamples.rds"))
  fit_path <- here("models", paste0("smote_intensity_", config_id, "_fit.rds"))

  wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(model_spec)

  tryCatch(
    {
      if (file.exists(resample_path) && file.exists(fit_path)) {
        resamples <- readRDS(resample_path)
        fitted_workflow <- readRDS(fit_path)
      } else {
        resamples <- fit_resamples(
          wf,
          resamples = folds,
          metrics = resample_metrics,
          control = control
        )
        fitted_workflow <- fit(wf, data = train_data)
        saveRDS(resamples, resample_path)
        saveRDS(fitted_workflow, fit_path)
      }

      oof_predictions <- collect_predictions(resamples)
      threshold_row <- choose_threshold_from_oof(oof_predictions)
      test_metrics <- evaluate_on_test(fitted_workflow, threshold_row$threshold)

      test_metrics %>%
        mutate(
          config_id = config_id,
          model = model_name,
          balancing_strategy = balancing_strategy,
          status = "trained",
          reason = NA_character_,
          .before = 1
        )
    },
    error = function(e) {
      tibble(
        config_id = config_id,
        model = model_name,
        balancing_strategy = balancing_strategy,
        status = "failed",
        reason = conditionMessage(e),
        threshold = NA_real_,
        accuracy = NA_real_,
        precision = NA_real_,
        recall = NA_real_,
        f1_score = NA_real_,
        roc_auc = NA_real_,
        pr_auc = NA_real_,
        predicted_positive = NA_integer_,
        false_positives = NA_integer_,
        false_negatives = NA_integer_,
        true_positives = NA_integer_,
        true_negatives = NA_integer_
      )
    }
  )
}

model_comparison_smote_intensity <- purrr::imap_dfr(strategy_recipes, function(rec, strategy_name) {
  purrr::imap_dfr(model_specs, function(model_spec, model_name) {
    fit_and_evaluate(model_name, strategy_name, model_spec, rec)
  })
})

catboost_available <- requireNamespace("catboost", quietly = TRUE)
catboost_rows <- tidyr::expand_grid(
  model = "catboost",
  balancing_strategy = names(strategy_recipes)
) %>%
  mutate(
    config_id = paste(model, balancing_strategy, sep = "__"),
    status = if (catboost_available) "failed" else "skipped",
    reason = if (catboost_available) {
      "CatBoost detecte mais aucun moteur tidymodels stable n'est configure dans ce pipeline."
    } else {
      "Package catboost indisponible pour cette version de R via CRAN ; configuration conservee mais non executee."
    },
    threshold = NA_real_,
    accuracy = NA_real_,
    precision = NA_real_,
    recall = NA_real_,
    f1_score = NA_real_,
    roc_auc = NA_real_,
    pr_auc = NA_real_,
    predicted_positive = NA_integer_,
    false_positives = NA_integer_,
    false_negatives = NA_integer_,
    true_positives = NA_integer_,
    true_negatives = NA_integer_
  ) %>%
  select(names(model_comparison_smote_intensity))

model_comparison_smote_intensity <- bind_rows(model_comparison_smote_intensity, catboost_rows) %>%
  arrange(model, balancing_strategy)

write_csv(
  model_comparison_smote_intensity,
  here("reports", "model_comparison_smote_intensity.csv")
)
print(model_comparison_smote_intensity)

compute_lift_table <- function(fitted_workflow, balancing_strategy) {
  prob_pred <- predict(fitted_workflow, new_data = test_data, type = "prob")
  scored <- bind_cols(
    test_data %>% select(claim_status),
    prob_pred
  ) %>%
    mutate(claim_flag = as.integer(claim_status == "yes")) %>%
    arrange(desc(.pred_yes))

  total_claims <- sum(scored$claim_flag)
  baseline_claim_rate <- mean(scored$claim_flag)
  total_n <- nrow(scored)

  purrr::map_dfr(c(0.05, 0.10, 0.20), function(segment_share) {
    n_segment <- ceiling(total_n * segment_share)
    segment_data <- scored %>% slice_head(n = n_segment)
    n_claims <- sum(segment_data$claim_flag)
    claim_rate <- n_claims / n_segment

    tibble(
      model = "xgboost",
      balancing_strategy = balancing_strategy,
      segment = paste0("top_", round(segment_share * 100), "_percent"),
      n_observations = n_segment,
      n_claims_captured = n_claims,
      share_of_total_claims_captured = n_claims / total_claims,
      claim_rate_in_segment = claim_rate,
      baseline_claim_rate = baseline_claim_rate,
      lift = claim_rate / baseline_claim_rate
    )
  })
}

xgboost_lift_by_smote_intensity <- purrr::map_dfr(names(strategy_recipes), function(strategy_name) {
  fit_path <- here("models", paste0("smote_intensity_xgboost__", strategy_name, "_fit.rds"))
  if (!file.exists(fit_path)) {
    return(tibble(
      model = "xgboost",
      balancing_strategy = strategy_name,
      segment = c("top_5_percent", "top_10_percent", "top_20_percent"),
      n_observations = NA_integer_,
      n_claims_captured = NA_integer_,
      share_of_total_claims_captured = NA_real_,
      claim_rate_in_segment = NA_real_,
      baseline_claim_rate = mean(test_data$claim_status == "yes"),
      lift = NA_real_
    ))
  }

  compute_lift_table(readRDS(fit_path), strategy_name)
})

write_csv(
  xgboost_lift_by_smote_intensity,
  here("reports", "xgboost_lift_by_smote_intensity.csv")
)
print(xgboost_lift_by_smote_intensity)

message(
  "Analyse SMOTE intensite terminee : reports/model_comparison_smote_intensity.csv, ",
  "reports/smote_intensity_class_balance.csv, reports/xgboost_lift_by_smote_intensity.csv"
)
