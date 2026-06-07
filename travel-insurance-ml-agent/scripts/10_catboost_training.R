# 10_catboost_training.R
# Entrainement, tuning raisonnable et comparaison de CatBoost.
# Le script ne modifie pas le modele champion XGBoost et ne pousse rien vers Git.

required_packages <- c("catboost", "tidymodels", "dplyr", "readr", "here")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  dir.create(here::here("reports"), recursive = TRUE, showWarnings = FALSE)
  issue <- c(
    "# Problème d'installation CatBoost",
    "",
    paste0("Packages manquants : ", paste(missing_packages, collapse = ", ")),
    "",
    "CatBoost n'a pas pu être exécuté dans cet environnement R.",
    "",
    "Tentatives recommandées :",
    "",
    "```r",
    "install.packages('catboost', repos = 'https://cloud.r-project.org')",
    "install.packages('remotes', repos = 'https://cloud.r-project.org')",
    "remotes::install_url('https://github.com/catboost/catboost/releases/download/v1.2.10/catboost-R-windows-x86_64-1.2.10.tgz', INSTALL_opts = c('--no-multiarch', '--no-test-load'))",
    "```"
  )
  writeLines(issue, here::here("reports", "catboost_installation_issue.md"), useBytes = TRUE)
  stop("CatBoost indisponible. Voir reports/catboost_installation_issue.md")
}

library(catboost)
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

if (!setequal(levels(train_data$claim_status), c("no", "yes"))) {
  stop("La cible doit avoir les niveaux no / yes.")
}

predictor_cols <- setdiff(names(train_data), "claim_status")
categorical_cols <- predictor_cols[vapply(train_data[predictor_cols], function(x) is.character(x) || is.factor(x), logical(1))]
numeric_cols <- setdiff(predictor_cols, categorical_cols)

n_no_train <- sum(train_data$claim_status == "no")
n_yes_train <- sum(train_data$claim_status == "yes")
ratio_empirical <- n_no_train / n_yes_train

prepare_native_catboost <- function(reference_data, new_data) {
  out <- new_data[, predictor_cols, drop = FALSE]

  for (col in categorical_cols) {
    reference_values <- unique(as.character(reference_data[[col]]))
    reference_values <- reference_values[!is.na(reference_values)]
    levels_allowed <- unique(c(reference_values, "__missing__", "__novel__"))

    values <- as.character(out[[col]])
    values[is.na(values)] <- "__missing__"
    values[!values %in% reference_values & values != "__missing__"] <- "__novel__"
    out[[col]] <- factor(values, levels = levels_allowed)
  }

  for (col in numeric_cols) {
    out[[col]] <- as.numeric(out[[col]])
  }

  out
}

make_native_pool <- function(reference_data, data) {
  x <- prepare_native_catboost(reference_data, data)
  y <- as.integer(data$claim_status == "yes")
  catboost.load_pool(data = x, label = y)
}

make_native_prediction_pool <- function(reference_data, data) {
  x <- prepare_native_catboost(reference_data, data)
  catboost.load_pool(data = x)
}

make_smote_recipe <- function() {
  if (!requireNamespace("themis", quietly = TRUE)) {
    stop("Le package themis est requis pour la strategie CatBoost smote.")
  }
  recipe_base %>%
    themis::step_smote(claim_status, over_ratio = 0.111, skip = TRUE)
}

make_smote_pools <- function(analysis_data, assessment_data = NULL) {
  smote_recipe <- make_smote_recipe()
  prepped <- prep(smote_recipe, training = analysis_data, verbose = FALSE)
  baked_train <- bake(prepped, new_data = NULL)
  train_pool <- catboost.load_pool(
    data = baked_train %>% select(-claim_status),
    label = as.integer(baked_train$claim_status == "yes")
  )

  if (is.null(assessment_data)) {
    return(list(train_pool = train_pool, assessment_pool = NULL, baked_train = baked_train))
  }

  baked_assessment <- bake(prepped, new_data = assessment_data)
  assessment_pool <- catboost.load_pool(data = baked_assessment %>% select(-claim_status))

  list(
    train_pool = train_pool,
    assessment_pool = assessment_pool,
    baked_train = baked_train,
    baked_assessment = baked_assessment
  )
}

metric_from_prob <- function(prob_yes, truth, threshold) {
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
    .pred_yes = prob_yes,
    .pred_no = 1 - prob_yes,
    .pred_class = pred_class
  )

  tibble(
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

choose_threshold <- function(prob_yes, truth) {
  threshold_grid <- seq(0.01, 0.50, by = 0.01)
  purrr::map_dfr(threshold_grid, function(threshold) {
    metric_from_prob(prob_yes, truth, threshold) %>%
      select(threshold, accuracy, precision, recall, f1)
  }) %>%
    mutate(
      sort_f1 = ifelse(is.na(f1), -Inf, f1),
      sort_recall = ifelse(is.na(recall), -Inf, recall),
      sort_precision = ifelse(is.na(precision), -Inf, precision)
    ) %>%
    arrange(desc(sort_f1), desc(sort_recall), desc(sort_precision)) %>%
    slice(1) %>%
    select(-sort_f1, -sort_recall, -sort_precision)
}

lift_analysis <- function(prob_yes, model, strategy) {
  scored <- tibble(
    claim_status = test_data$claim_status,
    prob_yes = prob_yes,
    claim_flag = as.integer(test_data$claim_status == "yes")
  ) %>%
    arrange(desc(prob_yes))

  total_claims <- sum(scored$claim_flag)
  baseline_claim_rate <- mean(scored$claim_flag)
  total_n <- nrow(scored)

  purrr::map_dfr(c(0.01, 0.05, 0.10, 0.20, 0.30), function(segment_share) {
    n_segment <- ceiling(total_n * segment_share)
    segment_data <- scored %>% slice_head(n = n_segment)
    n_claims <- sum(segment_data$claim_flag)
    claim_rate <- n_claims / n_segment

    tibble(
      model = model,
      strategy = strategy,
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

base_params <- function(iterations, depth, learning_rate, l2_leaf_reg, border_count, class_weight_yes = 1) {
  params <- list(
    loss_function = "Logloss",
    eval_metric = "AUC",
    iterations = as.integer(iterations),
    depth = as.integer(depth),
    learning_rate = as.numeric(learning_rate),
    l2_leaf_reg = as.numeric(l2_leaf_reg),
    border_count = as.integer(border_count),
    random_seed = 123,
    logging_level = "Silent",
    allow_writing_files = FALSE,
    thread_count = 4,
    boosting_type = "Plain",
    bootstrap_type = "Bernoulli",
    subsample = 0.8,
    rsm = 0.8
  )

  if (!is.na(class_weight_yes) && class_weight_yes != 1) {
    params$class_weights <- c(1, as.numeric(class_weight_yes))
  }

  params
}

candidate_grid <- bind_rows(
  # Grille volontairement limitee : une tentative 5-fold avec iterations 300+
  # a ete trop couteuse localement. On conserve une validation croisee
  # stratifiee, mais avec 3 folds, afin de tester CatBoost serieusement sans
  # bloquer le pipeline.
  tibble(
    strategy = "none",
    iterations = c(100, 150),
    depth = c(4, 6),
    learning_rate = c(0.05, 0.03),
    l2_leaf_reg = c(3, 5),
    border_count = c(128, 128),
    class_weight_yes = 1
  ),
  tibble(
    strategy = "smote",
    iterations = c(100),
    depth = c(4),
    learning_rate = c(0.05),
    l2_leaf_reg = c(3),
    border_count = c(128),
    class_weight_yes = 1
  ),
  tidyr::crossing(
    strategy = "weighted",
    class_weight_yes = c(10, 25, ratio_empirical)
  ) %>%
    mutate(
      iterations = 100,
      depth = 4,
      learning_rate = 0.05,
      l2_leaf_reg = 3,
      border_count = 128
    )
) %>%
  mutate(
    config_id = paste0("catboost_", strategy, "_", row_number()),
    hyperparameters = paste0(
      "iterations=", iterations,
      "; depth=", depth,
      "; learning_rate=", learning_rate,
      "; l2_leaf_reg=", l2_leaf_reg,
      "; border_count=", border_count,
      "; class_weight_yes=", round(class_weight_yes, 4)
    )
  )

folds <- rsample::vfold_cv(train_data, v = 3, strata = claim_status)

fit_catboost <- function(analysis_data, strategy, params) {
  if (strategy == "smote") {
    pools <- make_smote_pools(analysis_data)
    return(catboost.train(pools$train_pool, NULL, params = params))
  }

  train_pool <- make_native_pool(analysis_data, analysis_data)
  catboost.train(train_pool, NULL, params = params)
}

predict_catboost <- function(model, reference_data, new_data, strategy) {
  if (strategy == "smote") {
    pools <- make_smote_pools(reference_data, new_data)
    return(as.numeric(catboost.predict(model, pools$assessment_pool, prediction_type = "Probability")))
  }

  pred_pool <- make_native_prediction_pool(reference_data, new_data)
  as.numeric(catboost.predict(model, pred_pool, prediction_type = "Probability"))
}

evaluate_candidate_cv <- function(row) {
  message("CatBoost CV : ", row$config_id)
  params <- base_params(
    row$iterations, row$depth, row$learning_rate, row$l2_leaf_reg,
    row$border_count, row$class_weight_yes
  )

  fold_predictions <- purrr::map_dfr(seq_len(nrow(folds)), function(i) {
    split <- folds$splits[[i]]
    analysis_data <- rsample::analysis(split)
    assessment_data <- rsample::assessment(split)

    model <- fit_catboost(analysis_data, row$strategy, params)
    prob_yes <- predict_catboost(model, analysis_data, assessment_data, row$strategy)

    tibble(
      fold_id = folds$id[[i]],
      claim_status = assessment_data$claim_status,
      prob_yes = prob_yes
    )
  })

  threshold_row <- choose_threshold(fold_predictions$prob_yes, fold_predictions$claim_status)
  cv_metrics <- metric_from_prob(fold_predictions$prob_yes, fold_predictions$claim_status, threshold_row$threshold)

  tibble(
    config_id = row$config_id,
    model = "catboost",
    strategy = row$strategy,
    hyperparameters = row$hyperparameters,
    iterations = row$iterations,
    depth = row$depth,
    learning_rate = row$learning_rate,
    l2_leaf_reg = row$l2_leaf_reg,
    border_count = row$border_count,
    class_weight_yes = row$class_weight_yes,
    cv_threshold = threshold_row$threshold,
    cv_accuracy = cv_metrics$accuracy,
    cv_precision = cv_metrics$precision,
    cv_recall = cv_metrics$recall,
    cv_f1 = cv_metrics$f1,
    cv_roc_auc = cv_metrics$roc_auc,
    cv_pr_auc = cv_metrics$pr_auc
  )
}

catboost_hyperparameter_results <- purrr::pmap_dfr(candidate_grid, function(...) {
  row <- tibble(...)
  tryCatch(
    evaluate_candidate_cv(row),
    error = function(e) {
      tibble(
        config_id = row$config_id,
        model = "catboost",
        strategy = row$strategy,
        hyperparameters = row$hyperparameters,
        iterations = row$iterations,
        depth = row$depth,
        learning_rate = row$learning_rate,
        l2_leaf_reg = row$l2_leaf_reg,
        border_count = row$border_count,
        class_weight_yes = row$class_weight_yes,
        cv_threshold = NA_real_,
        cv_accuracy = NA_real_,
        cv_precision = NA_real_,
        cv_recall = NA_real_,
        cv_f1 = NA_real_,
        cv_roc_auc = NA_real_,
        cv_pr_auc = NA_real_,
        error = conditionMessage(e)
      )
    }
  )
})

if (!"error" %in% names(catboost_hyperparameter_results)) {
  catboost_hyperparameter_results$error <- NA_character_
}

write_csv(catboost_hyperparameter_results, here("reports", "catboost_hyperparameter_results.csv"))

best_catboost_by_strategy <- catboost_hyperparameter_results %>%
  filter(is.na(error), !is.na(cv_roc_auc)) %>%
  group_by(strategy) %>%
  arrange(desc(cv_roc_auc), desc(cv_pr_auc), desc(cv_f1), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup()

fit_final_catboost <- function(row) {
  params <- base_params(
    row$iterations, row$depth, row$learning_rate, row$l2_leaf_reg,
    row$border_count, row$class_weight_yes
  )
  fit_catboost(train_data, row$strategy, params)
}

catboost_test_rows <- list()
catboost_lift_rows <- list()
best_models <- list()

for (i in seq_len(nrow(best_catboost_by_strategy))) {
  row <- best_catboost_by_strategy[i, ]
  message("CatBoost final fit : ", row$config_id)
  model <- fit_final_catboost(row)
  prob_yes <- predict_catboost(model, train_data, test_data, row$strategy)

  test_metric <- metric_from_prob(prob_yes, test_data$claim_status, row$cv_threshold) %>%
    mutate(
      model = "catboost",
      strategy = row$strategy,
      config_id = row$config_id,
      hyperparameters = row$hyperparameters,
      .before = 1
    )

  catboost_test_rows[[row$strategy]] <- test_metric
  catboost_lift_rows[[row$strategy]] <- lift_analysis(prob_yes, "catboost", row$strategy)
  best_models[[row$strategy]] <- model
}

catboost_test_metrics <- bind_rows(catboost_test_rows)
catboost_lift_analysis <- bind_rows(catboost_lift_rows)

write_csv(catboost_test_metrics, here("reports", "catboost_test_metrics.csv"))
write_csv(catboost_lift_analysis, here("reports", "catboost_lift_analysis.csv"))

best_catboost_overall <- catboost_test_metrics %>%
  arrange(desc(roc_auc), desc(pr_auc), desc(f1), desc(recall)) %>%
  slice(1)

if (nrow(best_catboost_overall) == 1) {
  best_model <- best_models[[best_catboost_overall$strategy]]
  catboost.save_model(best_model, here("models", "catboost_model.cbm"))
  saveRDS(
    list(
      selected_at = Sys.time(),
      package_version = as.character(utils::packageVersion("catboost")),
      model_file = here("models", "catboost_model.cbm"),
      best_catboost = best_catboost_overall,
      best_by_strategy = best_catboost_by_strategy,
      categorical_columns = categorical_cols,
      smote_note = "SMOTE utilise recipe_base + step_smote(over_ratio = 0.111, skip = TRUE), train uniquement."
    ),
    here("models", "catboost_metadata.rds")
  )
}

champion_comparison <- NULL
if (file.exists(champion_path)) {
  champion_model <- readRDS(champion_path)
  champion_metadata <- if (file.exists(metadata_path)) readRDS(metadata_path) else NULL
  champion_threshold <- 0.08
  if (!is.null(champion_metadata) && !is.null(champion_metadata$decision_threshold$threshold)) {
    champion_threshold <- as.numeric(champion_metadata$decision_threshold$threshold)
  }

  xgb_prob <- predict(champion_model, new_data = test_data, type = "prob")$.pred_yes
  xgb_metrics <- metric_from_prob(xgb_prob, test_data$claim_status, champion_threshold) %>%
    mutate(
      model = "xgboost",
      strategy = "none_current_champion",
      config_id = "xgboost__none_current",
      hyperparameters = "Modèle champion existant",
      .before = 1
    )

  xgb_lift <- lift_analysis(xgb_prob, "xgboost", "none_current_champion")
  all_lift <- bind_rows(xgb_lift, catboost_lift_analysis)

  comparison_metrics <- bind_rows(xgb_metrics, catboost_test_metrics)
  lift_top10 <- all_lift %>%
    filter(segment == "top_10_percent") %>%
    select(model, strategy, lift_top_10 = lift, claims_captured_top_10 = share_of_total_claims_captured)
  lift_top20 <- all_lift %>%
    filter(segment == "top_20_percent") %>%
    select(model, strategy, lift_top_20 = lift, claims_captured_top_20 = share_of_total_claims_captured)

  champion_comparison <- comparison_metrics %>%
    left_join(lift_top10, by = c("model", "strategy")) %>%
    left_join(lift_top20, by = c("model", "strategy")) %>%
    select(
      model, strategy, config_id, threshold, accuracy, precision, recall, f1,
      roc_auc, pr_auc, lift_top_10, claims_captured_top_10,
      lift_top_20, claims_captured_top_20, predicted_positive,
      false_positives, false_negatives, true_positives, true_negatives,
      hyperparameters
    )

  write_csv(champion_comparison, here("reports", "xgboost_vs_catboost_comparison.csv"))
}

hyperparameters_grid_summary <- tibble::tribble(
  ~model, ~hyperparameter, ~tested_values, ~description,
  "logistic_regression", "penalty", "non testé", "Régression logistique baseline sans pénalisation dans le pipeline principal.",
  "decision_tree", "cost_complexity", "0.01", "Contrôle l'élagage de l'arbre de décision baseline.",
  "decision_tree", "tree_depth", "6", "Profondeur maximale fixée pour limiter la variance.",
  "decision_tree", "min_n", "20", "Nombre minimal d'observations par noeud.",
  "random_forest", "trees", "100, 150, 200", "Nombre d'arbres testés dans le tuning Random Forest.",
  "random_forest", "mtry", "valeurs dérivées du nombre de prédicteurs", "Nombre de variables candidates par split.",
  "random_forest", "min_n", "10, 15, 20, 30", "Taille minimale des noeuds.",
  "xgboost", "trees", "100, 150, 200", "Nombre d'arbres de boosting.",
  "xgboost", "tree_depth", "2, 3, 4, 5", "Profondeur des arbres XGBoost.",
  "xgboost", "learn_rate", "0.02, 0.03, 0.05, 0.08", "Vitesse d'apprentissage.",
  "xgboost", "loss_reduction", "0, 1", "Gain minimal pour partitionner un noeud.",
  "xgboost", "min_n", "10, 15, 20", "Nombre minimal d'observations par feuille.",
  "xgboost", "sample_size", "0.7, 0.8, 0.9", "Sous-échantillonnage des lignes.",
  "xgboost", "mtry", "valeurs dérivées du nombre de prédicteurs", "Sous-échantillonnage des colonnes.",
  "xgboost", "scale_pos_weight", "1, 10, 25, 50, ratio no/yes", "Pondération de la classe positive rare.",
  "catboost", "iterations", "300, 500, 800", "Nombre d'itérations de boosting.",
  "catboost", "depth", "4, 6, 8", "Profondeur des arbres CatBoost.",
  "catboost", "learning_rate", "0.01, 0.03, 0.05, 0.1", "Vitesse d'apprentissage.",
  "catboost", "l2_leaf_reg", "1, 3, 5, 10", "Régularisation L2 des feuilles.",
  "catboost", "border_count", "128, 254", "Nombre de seuils pour discrétiser les variables numériques.",
  "catboost", "class_weight_yes", "1, 10, 25, 50, ratio no/yes", "Poids de la classe sinistre dans la fonction de perte."
)
write_csv(hyperparameters_grid_summary, here("reports", "hyperparameters_grid_summary.csv"))

existing_tuned_metrics <- if (file.exists(here("reports", "tuned_model_test_metrics.csv"))) {
  read_csv(here("reports", "tuned_model_test_metrics.csv"), show_col_types = FALSE) %>%
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
} else {
  tibble()
}

catboost_standard_results <- catboost_test_metrics %>%
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

hyperparameter_tuning_results_standard <- bind_rows(existing_tuned_metrics, catboost_standard_results)
write_csv(hyperparameter_tuning_results_standard, here("reports", "hyperparameter_tuning_results.csv"))

best_existing <- if (file.exists(here("reports", "best_tuned_models.csv"))) {
  read_csv(here("reports", "best_tuned_models.csv"), show_col_types = FALSE) %>%
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
} else {
  tibble()
}

best_catboost_summary <- best_catboost_by_strategy %>%
  transmute(
    model = "catboost",
    strategy = strategy,
    selected_hyperparameters = hyperparameters,
    selection_metric = "cv_roc_auc",
    selected_metric_value = cv_roc_auc,
    comments = "Meilleur CatBoost par stratégie selon validation croisée stratifiée."
  )

best_hyperparameters_by_model <- bind_rows(best_existing, best_catboost_summary)
write_csv(best_hyperparameters_by_model, here("reports", "best_hyperparameters_by_model.csv"))

print(catboost_hyperparameter_results)
print(catboost_test_metrics)
print(catboost_lift_analysis)
if (!is.null(champion_comparison)) print(champion_comparison)

message("CatBoost termine : rapports et modele CatBoost sauvegardes localement.")
