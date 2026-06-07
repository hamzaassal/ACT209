# scripts/no_tariff/01_no_tariff_benchmark.R
# Analyse complementaire : modele underwriting sans variables tarifaires.
#
# Objectif methodologique :
# - retirer les variables net_sales et commision, qui peuvent deja concentrer
#   une information issue du processus tarifaire ou commercial ;
# - appliquer la meme logique que le benchmark principal : split train/test
#   existant, validation croisee stratifiee, seuil calibre hors test,
#   evaluation finale sur le test set inchange ;
# - comparer le champion de cette approche au champion du modele complet.

required_packages <- c("tidymodels", "dplyr", "readr", "ggplot2", "here", "rpart", "ranger", "xgboost")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Packages manquants : ", paste(missing_packages, collapse = ", "),
    "\nInstallez-les avec install.packages(...) puis relancez ce script."
  )
}

library(tidymodels)
library(dplyr)
library(readr)
library(ggplot2)
library(here)

set.seed(123)
options(yardstick.event_first = FALSE)

dir.create(here("models", "no_tariff"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("reports"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("reports", "figures"), recursive = TRUE, showWarnings = FALSE)

safe_write_csv <- function(x, path) {
  tmp <- tempfile(fileext = ".csv")
  readr::write_csv(x, tmp)
  ok <- tryCatch({
    file.copy(tmp, path, overwrite = TRUE)
  }, error = function(e) FALSE)

  if (!isTRUE(ok)) {
    fallback <- sub("\\.csv$", paste0("_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"), path)
    file.copy(tmp, fallback, overwrite = TRUE)
    warning("Impossible de remplacer ", path, ". Fichier de secours ecrit : ", fallback)
    return(invisible(fallback))
  }
  invisible(path)
}

train_path <- here("data", "processed", "train_data.rds")
test_path <- here("data", "processed", "test_data.rds")

if (!file.exists(train_path) || !file.exists(test_path)) {
  stop("Train/test absents. Executez d'abord scripts/03_preprocessing.R.")
}

train_data_full <- readRDS(train_path)
test_data_full <- readRDS(test_path)

if (!"claim_status" %in% names(train_data_full)) {
  stop("Variable cible introuvable : claim_status.")
}

tariff_variables_requested <- c("net_sales", "commision_in_value", "commision", "commission")
tariff_variables_removed <- intersect(tariff_variables_requested, names(train_data_full))

if (length(tariff_variables_removed) == 0) {
  warning("Aucune variable tarifaire attendue n'a ete trouvee. Le benchmark sans tarif sera identique au benchmark complet.")
}

train_data <- train_data_full %>% select(-any_of(tariff_variables_removed))
test_data <- test_data_full %>% select(-any_of(tariff_variables_removed))

if (!setequal(levels(train_data$claim_status), c("no", "yes"))) {
  stop("La cible doit avoir les niveaux no / yes.")
}

recipe_no_tariff <- recipe(claim_status ~ ., data = train_data) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors())

smote_available <- requireNamespace("themis", quietly = TRUE)
strategy_recipes <- list(none = recipe_no_tariff)
if (smote_available) {
  strategy_recipes$smote <- recipe_no_tariff %>%
    themis::step_smote(claim_status, over_ratio = 0.30, skip = TRUE)
}

prepped_recipe <- prep(recipe_no_tariff, training = train_data, verbose = FALSE)
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
  purrr::map_dfr(seq(0.01, 0.50, by = 0.01), function(threshold) {
    metric_from_prob(prob_yes, truth, threshold) %>%
      transmute(threshold, accuracy, precision, recall, f1)
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

folds <- vfold_cv(train_data, v = 5, strata = claim_status)
resample_metrics <- metric_set(accuracy, roc_auc)
control <- control_resamples(save_pred = TRUE, verbose = FALSE)

results <- list()
threshold_rows <- list()
test_rows <- list()
lift_rows <- list()

for (strategy_name in names(strategy_recipes)) {
  for (model_name in names(model_specs)) {
    config_id <- paste("no_tariff_clean", model_name, strategy_name, sep = "__")
    message("Benchmark sans variables tarifaires : ", config_id)

    wf <- workflow() %>%
      add_recipe(strategy_recipes[[strategy_name]]) %>%
      add_model(model_specs[[model_name]])

    resample_path <- here("models", "no_tariff", paste0(config_id, "_resamples.rds"))
    fit_path <- here("models", "no_tariff", paste0(config_id, "_fit.rds"))

    fitted <- tryCatch({
      if (file.exists(resample_path) && file.exists(fit_path)) {
        resamples <- readRDS(resample_path)
        final_fit <- readRDS(fit_path)
      } else {
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

      cv_pred <- collect_predictions(resamples)
      cv_threshold <- choose_threshold(cv_pred$.pred_yes, cv_pred$claim_status)
      cv_metrics <- metric_from_prob(cv_pred$.pred_yes, cv_pred$claim_status, cv_threshold$threshold)

      test_prob <- predict(final_fit, new_data = test_data, type = "prob")$.pred_yes
      test_metrics <- metric_from_prob(test_prob, test_data$claim_status, cv_threshold$threshold)

      threshold_rows[[config_id]] <- cv_threshold %>%
        mutate(config_id = config_id, model = model_name, strategy = strategy_name, .before = 1)

      test_rows[[config_id]] <- test_metrics %>%
        mutate(
          config_id = config_id,
          model = model_name,
          strategy = strategy_name,
          threshold = cv_threshold$threshold,
          .before = 1
        )

      lift_rows[[config_id]] <- lift_analysis(test_prob, model_name, strategy_name) %>%
        mutate(config_id = config_id, .before = 1)

      cv_metrics %>%
        mutate(
          config_id = config_id,
          model = model_name,
          strategy = strategy_name,
          status = "trained",
          reason = NA_character_,
          threshold = cv_threshold$threshold,
          .before = 1
        )
    }, error = function(e) {
      tibble(
        config_id = config_id,
        model = model_name,
        strategy = strategy_name,
        status = "failed",
        reason = conditionMessage(e),
        threshold = NA_real_,
        accuracy = NA_real_,
        precision = NA_real_,
        recall = NA_real_,
        f1 = NA_real_,
        roc_auc = NA_real_,
        pr_auc = NA_real_,
        predicted_positive = NA_real_,
        false_positives = NA_real_,
        false_negatives = NA_real_,
        true_positives = NA_real_,
        true_negatives = NA_real_
      )
    })

    results[[config_id]] <- fitted
  }
}

model_comparison <- bind_rows(results)
model_thresholds <- bind_rows(threshold_rows)
test_metrics <- bind_rows(test_rows)
lift_table <- bind_rows(lift_rows)

if (requireNamespace("catboost", quietly = TRUE)) {
  message("Benchmark sans variables tarifaires : CatBoost natif")

  predictor_cols <- setdiff(names(train_data), "claim_status")
  categorical_cols <- predictor_cols[vapply(train_data[predictor_cols], function(x) is.character(x) || is.factor(x), logical(1))]
  numeric_cols <- setdiff(predictor_cols, categorical_cols)
  ratio_empirical <- sum(train_data$claim_status == "no") / sum(train_data$claim_status == "yes")

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

  make_catboost_pool <- function(reference_data, data, with_label = TRUE) {
    x <- prepare_native_catboost(reference_data, data)
    if (with_label) {
      return(catboost::catboost.load_pool(data = x, label = as.integer(data$claim_status == "yes")))
    }
    catboost::catboost.load_pool(data = x)
  }

  fit_catboost_configuration <- function(strategy_name, class_weight_yes = 1) {
    config_id <- paste("no_tariff_clean", "catboost", strategy_name, sep = "__")
    message("Benchmark sans variables tarifaires : ", config_id)

    params <- list(
      loss_function = "Logloss",
      eval_metric = "AUC",
      iterations = 150,
      depth = 6,
      learning_rate = 0.05,
      l2_leaf_reg = 3,
      random_seed = 123,
      verbose = 0,
      thread_count = 2
    )

    if (!is.null(class_weight_yes) && class_weight_yes != 1) {
      params$class_weights <- c(1, class_weight_yes)
    }

    cv_pred <- purrr::map_dfr(seq_along(folds$splits), function(i) {
      split <- folds$splits[[i]]
      analysis_data <- rsample::analysis(split)
      assessment_data <- rsample::assessment(split)

      train_pool <- make_catboost_pool(analysis_data, analysis_data, with_label = TRUE)
      assessment_pool <- make_catboost_pool(analysis_data, assessment_data, with_label = FALSE)
      model <- catboost::catboost.train(train_pool, NULL, params = params)
      prob_yes <- catboost::catboost.predict(model, assessment_pool, prediction_type = "Probability")

      tibble(
        claim_status = assessment_data$claim_status,
        .pred_yes = as.numeric(prob_yes)
      )
    })

    cv_threshold <- choose_threshold(cv_pred$.pred_yes, cv_pred$claim_status)
    cv_metrics <- metric_from_prob(cv_pred$.pred_yes, cv_pred$claim_status, cv_threshold$threshold)

    train_pool <- make_catboost_pool(train_data, train_data, with_label = TRUE)
    test_pool <- make_catboost_pool(train_data, test_data, with_label = FALSE)
    final_model <- catboost::catboost.train(train_pool, NULL, params = params)
    test_prob <- as.numeric(catboost::catboost.predict(final_model, test_pool, prediction_type = "Probability"))
    test_metrics_row <- metric_from_prob(test_prob, test_data$claim_status, cv_threshold$threshold) %>%
      mutate(
        config_id = config_id,
        model = "catboost",
        strategy = strategy_name,
        threshold = cv_threshold$threshold,
        .before = 1
      )

    catboost::catboost.save_model(final_model, here("models", "no_tariff", paste0(config_id, ".cbm")))
    saveRDS(
      list(config_id = config_id, params = params, threshold = cv_threshold$threshold),
      here("models", "no_tariff", paste0(config_id, "_metadata.rds"))
    )

    threshold_rows[[config_id]] <<- cv_threshold %>%
      mutate(config_id = config_id, model = "catboost", strategy = strategy_name, .before = 1)
    test_rows[[config_id]] <<- test_metrics_row
    lift_rows[[config_id]] <<- lift_analysis(test_prob, "catboost", strategy_name) %>%
      mutate(config_id = config_id, .before = 1)

    cv_metrics %>%
      mutate(
        config_id = config_id,
        model = "catboost",
        strategy = strategy_name,
        status = "trained",
        reason = NA_character_,
        threshold = cv_threshold$threshold,
        .before = 1
      )
  }

  catboost_results <- bind_rows(
    tryCatch(
      fit_catboost_configuration("none", class_weight_yes = 1),
      error = function(e) tibble(
        config_id = "no_tariff_clean__catboost__none",
        model = "catboost",
        strategy = "none",
        status = "failed",
        reason = conditionMessage(e),
        threshold = NA_real_,
        accuracy = NA_real_,
        precision = NA_real_,
        recall = NA_real_,
        f1 = NA_real_,
        roc_auc = NA_real_,
        pr_auc = NA_real_,
        predicted_positive = NA_real_,
        false_positives = NA_real_,
        false_negatives = NA_real_,
        true_positives = NA_real_,
        true_negatives = NA_real_
      )
    ),
    tryCatch(
      fit_catboost_configuration("weighted", class_weight_yes = ratio_empirical),
      error = function(e) tibble(
        config_id = "no_tariff_clean__catboost__weighted",
        model = "catboost",
        strategy = "weighted",
        status = "failed",
        reason = conditionMessage(e),
        threshold = NA_real_,
        accuracy = NA_real_,
        precision = NA_real_,
        recall = NA_real_,
        f1 = NA_real_,
        roc_auc = NA_real_,
        pr_auc = NA_real_,
        predicted_positive = NA_real_,
        false_positives = NA_real_,
        false_negatives = NA_real_,
        true_positives = NA_real_,
        true_negatives = NA_real_
      )
    )
  )

  model_comparison <- bind_rows(model_comparison, catboost_results)
  model_thresholds <- bind_rows(threshold_rows)
  test_metrics <- bind_rows(test_rows)
  lift_table <- bind_rows(lift_rows)
} else {
  catboost_note <- "CatBoost indisponible dans cet environnement ; non execute dans le benchmark no_tariff."

  catboost_rows <- tidyr::expand_grid(
    model = "catboost",
    strategy = names(strategy_recipes)
  ) %>%
    mutate(
      config_id = paste("no_tariff_clean", model, strategy, sep = "__"),
      status = "skipped",
      reason = catboost_note,
      threshold = NA_real_,
      accuracy = NA_real_,
      precision = NA_real_,
      recall = NA_real_,
      f1 = NA_real_,
      roc_auc = NA_real_,
      pr_auc = NA_real_,
      predicted_positive = NA_real_,
      false_positives = NA_real_,
      false_negatives = NA_real_,
      true_positives = NA_real_,
      true_negatives = NA_real_
    )

  model_comparison <- bind_rows(model_comparison, catboost_rows)
}

model_ranking <- test_metrics %>%
  arrange(desc(roc_auc), desc(f1), desc(recall)) %>%
  mutate(rank = row_number())

if (nrow(model_ranking) == 0) {
  stop("Aucun modele no_tariff n'a pu etre evalue sur le test set.")
}

champion <- model_ranking %>% slice(1)
champion_model_path <- here("models", "no_tariff", paste0(champion$config_id, "_fit.rds"))
champion_object <- readRDS(champion_model_path)

saveRDS(champion_object, here("models", "no_tariff_best_model.rds"))
saveRDS(
  list(
    approach = "no_tariff",
    removed_variables = tariff_variables_removed,
    champion = champion,
    threshold = champion$threshold,
    selection_rule = "ROC AUC puis F1 puis recall sur la classe yes",
    methodological_note = "Les variables tarifaires sont retirees pour limiter la circularite entre tarification deja calculee et conseil underwriting."
  ),
  here("models", "no_tariff_model_metadata.rds")
)

safe_write_csv(model_comparison, here("reports", "no_tariff_model_comparison.csv"))
safe_write_csv(model_thresholds, here("reports", "no_tariff_model_thresholds.csv"))
safe_write_csv(test_metrics, here("reports", "no_tariff_test_metrics.csv"))
safe_write_csv(model_ranking, here("reports", "no_tariff_model_ranking.csv"))
safe_write_csv(lift_table, here("reports", "no_tariff_risk_ranking_lift_table.csv"))

full_champion <- NULL
full_selection_path <- here("reports", "final_model_selection_summary.csv")
if (file.exists(full_selection_path)) {
  full_champion <- read_csv(full_selection_path, show_col_types = FALSE) %>%
    arrange(final_rank) %>%
    slice(1) %>%
    transmute(
      approach = "modele_complet",
      champion_model = candidate_model,
      strategy = strategy,
      roc_auc = roc_auc,
      pr_auc = pr_auc,
      f1 = f1,
      recall = recall,
      precision = precision,
      lift_top_10 = lift_top_10,
      claims_captured_top_10 = claims_captured_top_10,
      lift_top_20 = lift_top_20,
      claims_captured_top_20 = claims_captured_top_20,
      note = "Champion historique avec variables tarifaires."
    )
}

no_tariff_lift_wide <- lift_table %>%
  filter(config_id == champion$config_id, segment %in% c("top_10_percent", "top_20_percent")) %>%
  select(segment, lift, share_of_total_claims_captured) %>%
  tidyr::pivot_wider(
    names_from = segment,
    values_from = c(lift, share_of_total_claims_captured)
  )

no_tariff_champion <- champion %>%
  bind_cols(no_tariff_lift_wide) %>%
  transmute(
    approach = "sans_variables_tarifaires",
    champion_model = model,
    strategy = strategy,
    roc_auc = roc_auc,
    pr_auc = pr_auc,
    f1 = f1,
    recall = recall,
    precision = precision,
    lift_top_10 = lift_top_10_percent,
    claims_captured_top_10 = share_of_total_claims_captured_top_10_percent,
    lift_top_20 = lift_top_20_percent,
    claims_captured_top_20 = share_of_total_claims_captured_top_20_percent,
    note = paste0(
      "Variables retirees : ",
      ifelse(length(tariff_variables_removed) == 0, "aucune", paste(tariff_variables_removed, collapse = ", "))
    )
  )

champion_comparison <- bind_rows(full_champion, no_tariff_champion)
safe_write_csv(champion_comparison, here("reports", "tariff_vs_no_tariff_champions.csv"))

p_compare <- champion_comparison %>%
  select(approach, roc_auc, pr_auc, f1, recall, precision, lift_top_10) %>%
  tidyr::pivot_longer(-approach, names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = metric, y = value, fill = approach)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Comparaison des champions : modele complet vs sans variables tarifaires",
    x = NULL,
    y = "Valeur"
  ) +
  theme_minimal()

ggsave(here("reports", "figures", "tariff_vs_no_tariff_champions.png"), p_compare, width = 8, height = 5)

message("Benchmark no_tariff termine.")
message("Variables tarifaires retirees : ", paste(tariff_variables_removed, collapse = ", "))
message("Champion no_tariff : ", champion$model, " / ", champion$strategy)
print(champion_comparison)
