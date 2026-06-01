# 09_hyperparameter_tuning.R
# Optimisation raisonnable des hyperparametres pour les modeles les plus
# prometteurs. Cette analyse est complementaire : elle ne remplace pas les
# resultats precedents et n'ecrase jamais models/best_model.rds.

required_packages <- c(
  "tidymodels", "dplyr", "readr", "here", "rpart", "ranger", "xgboost"
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
champion_path <- here("models", "best_model.rds")
metadata_path <- here("models", "model_metadata.rds")

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

n_no_train <- sum(train_data$claim_status == "no")
n_yes_train <- sum(train_data$claim_status == "yes")
scale_pos_weight_empirical <- n_no_train / n_yes_train

smote_available <- requireNamespace("themis", quietly = TRUE)
catboost_available <- requireNamespace("catboost", quietly = TRUE)

recipe_smote_light <- NULL
if (smote_available) {
  # SMOTE leger : environ 10 % de sinistres dans l'echantillon prepare.
  # skip = TRUE garantit que SMOTE n'est pas applique au test set.
  recipe_smote_light <- recipe_base %>%
    themis::step_smote(claim_status, over_ratio = 0.111, skip = TRUE)
}

prepped_recipe <- prep(recipe_base, training = train_data, verbose = FALSE)
n_predictors <- bake(prepped_recipe, new_data = train_data[0, ]) %>%
  select(-claim_status) %>%
  ncol()

mtry_values <- unique(pmax(1, pmin(n_predictors, c(
  floor(sqrt(n_predictors)),
  floor(n_predictors / 4),
  floor(n_predictors / 3),
  floor(n_predictors / 2)
))))

folds <- vfold_cv(train_data, v = 5, strata = claim_status)
resample_metrics <- metric_set(roc_auc, pr_auc)
control <- control_resamples(save_pred = TRUE, verbose = FALSE)

threshold_grid <- seq(0.01, 0.50, by = 0.01)

choose_threshold_from_oof <- function(predictions) {
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
      cv_accuracy_at_threshold = (tp + tn) / (tp + fp + fn + tn),
      cv_precision_at_threshold = precision_value,
      cv_recall_at_threshold = recall_value,
      cv_f1_at_threshold = f1_value
    )
  }) %>%
    mutate(
      sort_f1 = ifelse(is.na(cv_f1_at_threshold), -Inf, cv_f1_at_threshold),
      sort_recall = ifelse(is.na(cv_recall_at_threshold), -Inf, cv_recall_at_threshold),
      sort_precision = ifelse(is.na(cv_precision_at_threshold), -Inf, cv_precision_at_threshold)
    ) %>%
    arrange(desc(sort_f1), desc(sort_recall), desc(sort_precision)) %>%
    slice(1) %>%
    select(-sort_f1, -sort_recall, -sort_precision)
}

evaluate_probabilities <- function(prob_yes, truth, threshold) {
  pred_class <- factor(ifelse(prob_yes >= threshold, "yes", "no"), levels = c("no", "yes"))

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

  pred_data <- tibble(
    claim_status = truth,
    .pred_class = pred_class,
    .pred_yes = prob_yes,
    .pred_no = 1 - prob_yes
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

compute_lift_table <- function(prob_yes, model_label, model_variant) {
  scored <- tibble(
    claim_status = test_data$claim_status,
    .pred_yes = prob_yes,
    claim_flag = as.integer(test_data$claim_status == "yes")
  ) %>%
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
      model = model_label,
      model_variant = model_variant,
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

build_xgb_spec <- function(row, scale_pos_weight = 1) {
  trees_value <- as.integer(row[["trees"]])
  tree_depth_value <- as.integer(row[["tree_depth"]])
  learn_rate_value <- as.numeric(row[["learn_rate"]])
  loss_reduction_value <- as.numeric(row[["loss_reduction"]])
  min_n_value <- as.integer(row[["min_n"]])
  sample_size_value <- as.numeric(row[["sample_size"]])
  mtry_value <- as.integer(row[["mtry"]])
  scale_pos_weight_value <- as.numeric(scale_pos_weight)

  boost_tree(
    trees = trees_value,
    tree_depth = tree_depth_value,
    learn_rate = learn_rate_value,
    loss_reduction = loss_reduction_value,
    min_n = min_n_value,
    sample_size = sample_size_value,
    mtry = mtry_value
  ) %>%
    set_engine("xgboost", nthread = 2, verbosity = 0, scale_pos_weight = scale_pos_weight_value) %>%
    set_mode("classification")
}

build_rf_spec <- function(row) {
  trees_value <- as.integer(row[["trees"]])
  mtry_value <- as.integer(row[["mtry"]])
  min_n_value <- as.integer(row[["min_n"]])

  rand_forest(
    trees = trees_value,
    mtry = mtry_value,
    min_n = min_n_value
  ) %>%
    set_engine("ranger", importance = "impurity", probability = TRUE, num.threads = 2) %>%
    set_mode("classification")
}

xgb_grid <- tibble(
  trees = c(100, 150, 200, 150, 200, 100),
  tree_depth = c(3, 3, 4, 4, 5, 2),
  learn_rate = c(0.05, 0.03, 0.05, 0.03, 0.02, 0.08),
  loss_reduction = c(0, 0, 0, 1, 0, 0),
  min_n = c(10, 15, 10, 20, 10, 10),
  sample_size = c(0.8, 0.8, 0.7, 0.8, 0.7, 0.9),
  mtry = rep(mtry_values, length.out = 6)
)

xgb_weight_grid <- tidyr::crossing(
  scale_pos_weight = c(1, 10, 25, 50, scale_pos_weight_empirical),
  config_row = 1:2
) %>%
  mutate(
    trees = c(150, 200)[config_row],
    tree_depth = c(3, 4)[config_row],
    learn_rate = c(0.03, 0.05)[config_row],
    loss_reduction = c(0, 0)[config_row],
    min_n = c(15, 10)[config_row],
    sample_size = c(0.8, 0.7)[config_row],
    mtry = rep(mtry_values, length.out = n())
  ) %>%
  select(-config_row)

xgb_smote_grid <- xgb_grid %>% slice(1:4)

rf_grid <- tibble(
  trees = c(100, 150, 200, 150),
  min_n = c(10, 15, 20, 30),
  mtry = rep(mtry_values, length.out = 4)
)

candidate_configs <- list()

for (i in seq_len(nrow(xgb_grid))) {
  row <- xgb_grid[i, ]
  candidate_configs[[length(candidate_configs) + 1]] <- list(
    model = "xgboost",
    model_variant = "xgboost_none",
    config_id = paste0("xgboost_none_", i),
    recipe = recipe_base,
    spec = build_xgb_spec(row, scale_pos_weight = 1),
    params = row %>% mutate(scale_pos_weight = 1, smote_over_ratio = NA_real_)
  )
}

for (i in seq_len(nrow(xgb_weight_grid))) {
  row <- xgb_weight_grid[i, ]
  candidate_configs[[length(candidate_configs) + 1]] <- list(
    model = "xgboost",
    model_variant = "xgboost_weighted",
    config_id = paste0("xgboost_weighted_", i),
    recipe = recipe_base,
    spec = build_xgb_spec(row, scale_pos_weight = row$scale_pos_weight),
    params = row %>% mutate(smote_over_ratio = NA_real_)
  )
}

if (smote_available) {
  for (i in seq_len(nrow(xgb_smote_grid))) {
    row <- xgb_smote_grid[i, ]
    candidate_configs[[length(candidate_configs) + 1]] <- list(
      model = "xgboost",
      model_variant = "xgboost_smote_light",
      config_id = paste0("xgboost_smote_light_", i),
      recipe = recipe_smote_light,
      spec = build_xgb_spec(row, scale_pos_weight = 1),
      params = row %>% mutate(scale_pos_weight = 1, smote_over_ratio = 0.111)
    )
  }
}

for (i in seq_len(nrow(rf_grid))) {
  row <- rf_grid[i, ]
  candidate_configs[[length(candidate_configs) + 1]] <- list(
    model = "random_forest",
    model_variant = "random_forest",
    config_id = paste0("random_forest_", i),
    recipe = recipe_base,
    spec = build_rf_spec(row),
    params = row %>%
      mutate(
        tree_depth = NA_real_,
        learn_rate = NA_real_,
        loss_reduction = NA_real_,
        sample_size = NA_real_,
        scale_pos_weight = NA_real_,
        smote_over_ratio = NA_real_
      )
  )
}

tune_one_config <- function(config) {
  message("Tuning : ", config$config_id)

  resample_path <- here("models", paste0("tuning_", config$config_id, "_resamples.rds"))
  fit_path <- here("models", paste0("tuning_", config$config_id, "_fit.rds"))

  wf <- workflow() %>%
    add_recipe(config$recipe) %>%
    add_model(config$spec)

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

      predictions <- collect_predictions(resamples)
      threshold_row <- choose_threshold_from_oof(predictions)
      cv_roc_auc <- roc_auc(predictions, truth = claim_status, .pred_yes, event_level = "second")$.estimate
      cv_pr_auc <- pr_auc(predictions, truth = claim_status, .pred_yes, event_level = "second")$.estimate

      params <- config$params
      tibble(
        config_id = config$config_id,
        model = config$model,
        model_variant = config$model_variant,
        status = "trained",
        reason = NA_character_,
        cv_roc_auc = cv_roc_auc,
        cv_pr_auc = cv_pr_auc,
        cv_threshold = threshold_row$threshold,
        cv_accuracy_at_threshold = threshold_row$cv_accuracy_at_threshold,
        cv_precision_at_threshold = threshold_row$cv_precision_at_threshold,
        cv_recall_at_threshold = threshold_row$cv_recall_at_threshold,
        cv_f1_at_threshold = threshold_row$cv_f1_at_threshold,
        trees = params$trees,
        tree_depth = params$tree_depth,
        learn_rate = params$learn_rate,
        loss_reduction = params$loss_reduction,
        min_n = params$min_n,
        sample_size = params$sample_size,
        mtry = params$mtry,
        scale_pos_weight = params$scale_pos_weight,
        smote_over_ratio = params$smote_over_ratio,
        fit_path = fit_path
      )
    },
    error = function(e) {
      tibble(
        config_id = config$config_id,
        model = config$model,
        model_variant = config$model_variant,
        status = "failed",
        reason = conditionMessage(e),
        cv_roc_auc = NA_real_,
        cv_pr_auc = NA_real_,
        cv_threshold = NA_real_,
        cv_accuracy_at_threshold = NA_real_,
        cv_precision_at_threshold = NA_real_,
        cv_recall_at_threshold = NA_real_,
        cv_f1_at_threshold = NA_real_,
        trees = NA_real_,
        tree_depth = NA_real_,
        learn_rate = NA_real_,
        loss_reduction = NA_real_,
        min_n = NA_real_,
        sample_size = NA_real_,
        mtry = NA_real_,
        scale_pos_weight = NA_real_,
        smote_over_ratio = NA_real_,
        fit_path = NA_character_
      )
    }
  )
}

hyperparameter_tuning_results <- purrr::map_dfr(candidate_configs, tune_one_config)

if (catboost_available) {
  catboost_reason <- "CatBoost detecte, mais aucun moteur tidymodels stable n'est configure dans ce pipeline R. Tuning CatBoost a documenter separement via l'API native si necessaire."
} else {
  catboost_reason <- "Package catboost indisponible pour cette version de R via CRAN ; configuration conservee mais non executee."
}

catboost_row <- tibble(
  config_id = "catboost_tuning_skipped",
  model = "catboost",
  model_variant = "catboost",
  status = "skipped",
  reason = catboost_reason,
  cv_roc_auc = NA_real_,
  cv_pr_auc = NA_real_,
  cv_threshold = NA_real_,
  cv_accuracy_at_threshold = NA_real_,
  cv_precision_at_threshold = NA_real_,
  cv_recall_at_threshold = NA_real_,
  cv_f1_at_threshold = NA_real_,
  trees = NA_real_,
  tree_depth = NA_real_,
  learn_rate = NA_real_,
  loss_reduction = NA_real_,
  min_n = NA_real_,
  sample_size = NA_real_,
  mtry = NA_real_,
  scale_pos_weight = NA_real_,
  smote_over_ratio = NA_real_,
  fit_path = NA_character_
)

hyperparameter_tuning_results <- bind_rows(hyperparameter_tuning_results, catboost_row) %>%
  arrange(model_variant, desc(cv_roc_auc), desc(cv_pr_auc))

write_csv(hyperparameter_tuning_results, here("reports", "hyperparameter_tuning_cv_results.csv"))

best_tuned_models <- hyperparameter_tuning_results %>%
  filter(status == "trained") %>%
  group_by(model_variant) %>%
  arrange(desc(cv_roc_auc), desc(cv_pr_auc), desc(cv_f1_at_threshold), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(desc(cv_roc_auc), desc(cv_pr_auc), desc(cv_f1_at_threshold))

write_csv(best_tuned_models, here("reports", "best_tuned_models.csv"))

evaluate_tuned_model <- function(row) {
  fit <- readRDS(row$fit_path)
  prob_yes <- predict(fit, new_data = test_data, type = "prob")$.pred_yes
  evaluate_probabilities(prob_yes, test_data$claim_status, row$cv_threshold) %>%
    mutate(
      config_id = row$config_id,
      model = row$model,
      model_variant = row$model_variant,
      .before = 1
    )
}

tuned_model_test_metrics <- purrr::pmap_dfr(
  best_tuned_models,
  function(...) evaluate_tuned_model(tibble(...))
) %>%
  arrange(desc(roc_auc), desc(pr_auc), desc(f1_score))

write_csv(tuned_model_test_metrics, here("reports", "tuned_model_test_metrics.csv"))

tuned_model_lift_analysis <- purrr::pmap_dfr(
  best_tuned_models,
  function(...) {
    row <- tibble(...)
    fit <- readRDS(row$fit_path)
    prob_yes <- predict(fit, new_data = test_data, type = "prob")$.pred_yes
    compute_lift_table(prob_yes, row$model, row$model_variant)
  }
)

write_csv(tuned_model_lift_analysis, here("reports", "tuned_model_lift_analysis.csv"))

champion_metadata <- if (file.exists(metadata_path)) readRDS(metadata_path) else NULL
champion_threshold <- 0.08
if (!is.null(champion_metadata) && !is.null(champion_metadata$decision_threshold$threshold)) {
  champion_threshold <- as.numeric(champion_metadata$decision_threshold$threshold)
}

champion_comparison <- NULL
if (file.exists(champion_path)) {
  champion_fit <- readRDS(champion_path)
  champion_prob <- predict(champion_fit, new_data = test_data, type = "prob")$.pred_yes
  champion_metrics <- evaluate_probabilities(champion_prob, test_data$claim_status, champion_threshold) %>%
    mutate(
      comparison_role = "current_champion",
      config_id = "xgboost__none_current",
      model = "xgboost",
      model_variant = "xgboost_none_current",
      .before = 1
    )

  champion_lift <- compute_lift_table(champion_prob, "xgboost", "xgboost_none_current")
  champion_top10 <- champion_lift %>% filter(segment == "top_10_percent")
  champion_top20 <- champion_lift %>% filter(segment == "top_20_percent")

  best_tuned_metric <- tuned_model_test_metrics %>%
    arrange(desc(roc_auc), desc(pr_auc), desc(f1_score)) %>%
    slice(1) %>%
    mutate(comparison_role = "best_tuned")

  best_tuned_lift <- tuned_model_lift_analysis %>%
    filter(model_variant == best_tuned_metric$model_variant)
  best_tuned_top10 <- best_tuned_lift %>% filter(segment == "top_10_percent")
  best_tuned_top20 <- best_tuned_lift %>% filter(segment == "top_20_percent")

  champion_comparison <- bind_rows(
    champion_metrics %>%
      mutate(
        lift_top_10 = champion_top10$lift,
        claims_captured_top_10 = champion_top10$share_of_total_claims_captured,
        claims_captured_top_20 = champion_top20$share_of_total_claims_captured
      ),
    best_tuned_metric %>%
      mutate(
        lift_top_10 = best_tuned_top10$lift,
        claims_captured_top_10 = best_tuned_top10$share_of_total_claims_captured,
        claims_captured_top_20 = best_tuned_top20$share_of_total_claims_captured
      )
  ) %>%
    select(
      comparison_role, config_id, model, model_variant, threshold, accuracy,
      precision, recall, f1_score, roc_auc, pr_auc, lift_top_10,
      claims_captured_top_10, claims_captured_top_20, predicted_positive,
      false_positives, false_negatives, true_positives, true_negatives
    )

  write_csv(champion_comparison, here("reports", "champion_vs_tuned_comparison.csv"))

  tuned_is_better <- best_tuned_metric$roc_auc > champion_metrics$roc_auc ||
    (
      isTRUE(all.equal(best_tuned_metric$roc_auc, champion_metrics$roc_auc)) &&
        best_tuned_metric$pr_auc > champion_metrics$pr_auc
    )

  if (isTRUE(tuned_is_better)) {
    best_tuned_row <- best_tuned_models %>%
      filter(config_id == best_tuned_metric$config_id) %>%
      slice(1)
    best_tuned_fit <- readRDS(best_tuned_row$fit_path)

    saveRDS(best_tuned_fit, here("models", "best_tuned_model.rds"))
    saveRDS(
      list(
        selected_at = Sys.time(),
        selection_rule = "ROC AUC test, puis PR AUC test",
        best_tuned_model = best_tuned_row,
        test_metrics = best_tuned_metric,
        lift_analysis = best_tuned_lift,
        champion_comparison = champion_comparison
      ),
      here("models", "best_tuned_model_metadata.rds")
    )
  }
} else {
  warning("models/best_model.rds introuvable : comparaison champion vs tuning non produite.")
}

print(hyperparameter_tuning_results)
print(best_tuned_models)
print(tuned_model_test_metrics)
print(tuned_model_lift_analysis)
if (!is.null(champion_comparison)) print(champion_comparison)

existing_tuned_metrics <- tuned_model_test_metrics %>%
  transmute(
    model = model,
    strategy = model_variant,
    hyperparameters = config_id,
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    f1 = f1_score,
    roc_auc = roc_auc,
    pr_auc = pr_auc,
    threshold = threshold,
    notes = "Résultat issu de scripts/09_hyperparameter_tuning.R"
  )

catboost_standard_results <- if (file.exists(here("reports", "catboost_test_metrics.csv"))) {
  read_csv(here("reports", "catboost_test_metrics.csv"), show_col_types = FALSE) %>%
    transmute(
      model = model,
      strategy = strategy,
      hyperparameters = hyperparameters,
      accuracy = accuracy,
      precision = precision,
      recall = recall,
      f1 = f1,
      roc_auc = roc_auc,
      pr_auc = pr_auc,
      threshold = threshold,
      notes = "CatBoost natif ; SMOTE uniquement sur train pour la stratégie smote."
    )
} else {
  tibble()
}

write_csv(
  bind_rows(existing_tuned_metrics, catboost_standard_results),
  here("reports", "hyperparameter_tuning_results.csv")
)

best_existing <- best_tuned_models %>%
  transmute(
    model = model,
    strategy = model_variant,
    selected_hyperparameters = paste0(
      "config_id=", config_id,
      "; trees=", trees,
      "; tree_depth=", tree_depth,
      "; learn_rate=", learn_rate,
      "; min_n=", min_n,
      "; mtry=", mtry,
      "; scale_pos_weight=", scale_pos_weight,
      "; smote_over_ratio=", smote_over_ratio
    ),
    selection_metric = "cv_roc_auc",
    selected_metric_value = cv_roc_auc,
    comments = "Meilleur par famille dans le tuning XGBoost/Random Forest."
  )

best_catboost_summary <- if (file.exists(here("reports", "catboost_hyperparameter_results.csv"))) {
  read_csv(here("reports", "catboost_hyperparameter_results.csv"), show_col_types = FALSE) %>%
    filter(is.na(error), !is.na(cv_roc_auc)) %>%
    group_by(strategy) %>%
    arrange(desc(cv_roc_auc), desc(cv_pr_auc), desc(cv_f1), .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    transmute(
      model = "catboost",
      strategy = strategy,
      selected_hyperparameters = hyperparameters,
      selection_metric = "cv_roc_auc",
      selected_metric_value = cv_roc_auc,
      comments = "Meilleur CatBoost par stratégie selon validation croisée stratifiée."
    )
} else {
  tibble()
}

write_csv(
  bind_rows(best_existing, best_catboost_summary),
  here("reports", "best_hyperparameters_by_model.csv")
)

message("Tuning termine : resultats sauvegardes dans reports/ et, si applicable, models/best_tuned_model.rds.")
