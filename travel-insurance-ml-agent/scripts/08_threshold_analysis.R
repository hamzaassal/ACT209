# 08_threshold_analysis.R
# Analyse de seuils sur le test set pour le modele champion XGBoost.
# Cette analyse est descriptive : elle sert a comprendre le compromis
# precision/recall sur le test set, pas a re-selectionner le modele.

required_packages <- c("tidymodels", "dplyr", "readr", "ggplot2", "here")
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

dir.create(here("reports"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("reports", "figures"), recursive = TRUE, showWarnings = FALSE)

model_path <- here("models", "best_model.rds")
metadata_path <- here("models", "model_metadata.rds")
test_path <- here("data", "processed", "test_data.rds")

if (!file.exists(model_path)) {
  stop("Modele champion introuvable : models/best_model.rds")
}

if (!file.exists(test_path)) {
  stop("Test set introuvable : data/processed/test_data.rds")
}

best_model <- readRDS(model_path)
metadata <- if (file.exists(metadata_path)) readRDS(metadata_path) else NULL
test_data <- readRDS(test_path)

champion_info <- if (!is.null(metadata) && !is.null(metadata$champion)) metadata$champion else NULL
champion_config_id <- if (!is.null(champion_info) && "config_id" %in% names(champion_info)) as.character(champion_info$config_id[1]) else "current_champion"
champion_model_name <- if (!is.null(champion_info) && "model" %in% names(champion_info)) as.character(champion_info$model[1]) else NA_character_
champion_strategy <- if (!is.null(champion_info) && "strategy" %in% names(champion_info)) as.character(champion_info$strategy[1]) else NA_character_

if (!"claim_status" %in% names(test_data)) {
  stop("Variable cible introuvable dans le test set : claim_status")
}

prob_pred <- predict(best_model, new_data = test_data, type = "prob")

if (!".pred_yes" %in% names(prob_pred)) {
  stop("Colonne .pred_yes introuvable dans les predictions du modele.")
}

threshold_grid <- seq(0.01, 0.60, by = 0.01)
n_test <- nrow(test_data)

threshold_analysis <- purrr::map_dfr(threshold_grid, function(threshold) {
  pred_class <- factor(
    ifelse(prob_pred$.pred_yes >= threshold, "yes", "no"),
    levels = c("no", "yes")
  )

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

  positives_predicted <- tp + fp

  tibble(
    threshold = threshold,
    accuracy = (tp + tn) / n_test,
    precision = precision_value,
    recall = recall_value,
    f1_score = f1_value,
    positives_predicted = positives_predicted,
    positive_prediction_rate = positives_predicted / n_test,
    false_positives = fp,
    false_negatives = fn,
    true_positives = tp,
    true_negatives = tn
  )
}) %>%
  mutate(
    config_id = champion_config_id,
    model = champion_model_name,
    strategy = champion_strategy,
    .before = threshold
  )

statistical_threshold <- threshold_analysis %>%
  mutate(sort_f1 = ifelse(is.na(f1_score), -Inf, f1_score)) %>%
  arrange(desc(sort_f1), desc(recall), desc(precision)) %>%
  slice(1) %>%
  mutate(threshold_type = "statistical_f1_max") %>%
  select(threshold_type, everything(), -sort_f1)

business_threshold <- threshold_analysis %>%
  filter(recall >= 0.50) %>%
  arrange(desc(precision), desc(f1_score), threshold) %>%
  slice(1) %>%
  mutate(threshold_type = "business_recall_min_50") %>%
  select(threshold_type, everything())

if (nrow(business_threshold) == 0) {
  business_threshold <- tibble(
    threshold_type = "business_recall_min_50",
    threshold = NA_real_,
    accuracy = NA_real_,
    precision = NA_real_,
    recall = NA_real_,
    f1_score = NA_real_,
    positives_predicted = NA_integer_,
    positive_prediction_rate = NA_real_,
    false_positives = NA_integer_,
    false_negatives = NA_integer_,
    true_positives = NA_integer_,
    true_negatives = NA_integer_
  )
}

selected_thresholds <- bind_rows(statistical_threshold, business_threshold)

write_csv(threshold_analysis, here("reports", "threshold_analysis.csv"))
write_csv(selected_thresholds, here("reports", "selected_thresholds.csv"))

if (!is.null(metadata)) {
  metadata$threshold_analysis <- list(
    grid = threshold_grid,
    statistical_threshold = statistical_threshold,
    business_threshold = business_threshold,
    generated_at = Sys.time()
  )
  saveRDS(metadata, metadata_path)
}

p_f1 <- ggplot(threshold_analysis, aes(x = threshold, y = f1_score)) +
  geom_line(color = "#2b6cb0", linewidth = 1) +
  geom_point(color = "#2b6cb0", size = 1.8) +
  labs(title = "F1-score selon le seuil", x = "Seuil", y = "F1-score") +
  theme_minimal()

p_precision_recall <- threshold_analysis %>%
  select(threshold, precision, recall) %>%
  tidyr::pivot_longer(cols = c(precision, recall), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = threshold, y = value, color = metric)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  scale_color_manual(values = c(precision = "#2b6cb0", recall = "#c53030")) +
  labs(title = "Precision et recall selon le seuil", x = "Seuil", y = "Valeur", color = "Métrique") +
  theme_minimal()

p_fp_fn <- threshold_analysis %>%
  select(threshold, false_positives, false_negatives) %>%
  tidyr::pivot_longer(cols = c(false_positives, false_negatives), names_to = "error_type", values_to = "count") %>%
  ggplot(aes(x = threshold, y = count, color = error_type)) +
  geom_line(linewidth = 1) +
  geom_point(size = 1.6) +
  scale_color_manual(values = c(false_positives = "#dd6b20", false_negatives = "#805ad5")) +
  labs(title = "Faux positifs et faux négatifs selon le seuil", x = "Seuil", y = "Nombre", color = "Type d'erreur") +
  theme_minimal()

p_positive_count <- ggplot(threshold_analysis, aes(x = threshold, y = positives_predicted)) +
  geom_line(color = "#2f855a", linewidth = 1) +
  geom_point(color = "#2f855a", size = 1.8) +
  labs(title = "Nombre de positifs prédits selon le seuil", x = "Seuil", y = "Positifs prédits") +
  theme_minimal()

ggsave(here("reports", "figures", "threshold_f1_score.png"), p_f1, width = 7, height = 5)
ggsave(here("reports", "figures", "threshold_precision_recall.png"), p_precision_recall, width = 7, height = 5)
ggsave(here("reports", "figures", "threshold_false_positives_false_negatives.png"), p_fp_fn, width = 7, height = 5)
ggsave(here("reports", "figures", "threshold_positive_predictions.png"), p_positive_count, width = 7, height = 5)

print(selected_thresholds)
message("Analyse de seuils sauvegardee : reports/threshold_analysis.csv")
