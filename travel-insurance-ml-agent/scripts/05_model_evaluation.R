# 05_model_evaluation.R
# Evaluation finale du modele champion sur le test set non modifie.

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
options(yardstick.event_first = FALSE)

dir.create(here("reports"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("reports", "figures"), recursive = TRUE, showWarnings = FALSE)

ranking_path <- here("reports", "model_ranking.csv")
thresholds_path <- here("reports", "model_thresholds.csv")
train_path <- here("data", "processed", "train_data.rds")
test_path <- here("data", "processed", "test_data.rds")

if (!file.exists(ranking_path)) {
  stop("Classement absent : executez scripts/04_model_training.R.")
}
if (!file.exists(train_path) || !file.exists(test_path)) {
  stop("Train/test set absents : executez scripts/03_preprocessing.R.")
}

model_ranking <- read_csv(ranking_path, show_col_types = FALSE)
model_thresholds <- if (file.exists(thresholds_path)) read_csv(thresholds_path, show_col_types = FALSE) else NULL
train_data <- readRDS(train_path)
test_data <- readRDS(test_path)

if (nrow(model_ranking) == 0) {
  stop("Aucun modele entraine disponible dans model_ranking.csv.")
}

champion <- model_ranking %>% arrange(desc(roc_auc), desc(f_meas), desc(recall)) %>% slice(1)
best_config_id <- champion$config_id
best_model_path <- here("models", paste0(best_config_id, "_fit.rds"))

if (!file.exists(best_model_path)) {
  stop("Modele champion introuvable : ", best_model_path)
}

best_model <- readRDS(best_model_path)
message("Modele champion evalue : ", best_config_id)

threshold_grid <- seq(0.01, 0.50, by = 0.01)

# Le seuil 0,5 est souvent inadapte pour les sinistres rares. Depuis l'audit ML,
# le seuil final provient des predictions out-of-fold de la configuration
# championne, ce qui evite de le calibrer sur le test set.
if (!is.null(model_thresholds) && best_config_id %in% model_thresholds$config_id) {
  best_threshold <- model_thresholds %>%
    filter(config_id == best_config_id) %>%
    slice(1) %>%
    pull(threshold)
  threshold_metrics <- model_thresholds %>% filter(config_id == best_config_id)
} else {
  train_prob <- predict(best_model, new_data = train_data, type = "prob")
  threshold_metrics <- purrr::map_dfr(threshold_grid, function(threshold) {
    pred_class <- factor(ifelse(train_prob$.pred_yes >= threshold, "yes", "no"), levels = c("no", "yes"))
    train_eval <- tibble(claim_status = train_data$claim_status, .pred_class = pred_class, .pred_yes = train_prob$.pred_yes)

    tibble(
      threshold = threshold,
      f1 = f_meas(train_eval, truth = claim_status, estimate = .pred_class, event_level = "second")$.estimate,
      recall = recall(train_eval, truth = claim_status, estimate = .pred_class, event_level = "second")$.estimate,
      precision = precision(train_eval, truth = claim_status, estimate = .pred_class, event_level = "second")$.estimate
    )
  })

  best_threshold <- threshold_metrics %>%
    mutate(f1 = ifelse(is.na(f1), -Inf, f1), precision = ifelse(is.na(precision), 0, precision)) %>%
    arrange(desc(f1), desc(recall), desc(precision)) %>%
    slice(1) %>%
    pull(threshold)
}

prob_pred <- predict(best_model, new_data = test_data, type = "prob")
class_pred <- tibble(
  .pred_class = factor(ifelse(prob_pred$.pred_yes >= best_threshold, "yes", "no"), levels = c("no", "yes"))
)

evaluation_data <- bind_cols(
  test_data %>% select(claim_status),
  class_pred,
  prob_pred
)

if (!".pred_yes" %in% names(evaluation_data)) {
  stop("Colonne .pred_yes introuvable. Verifiez les niveaux de claim_status.")
}

final_metrics <- bind_rows(
  accuracy(evaluation_data, truth = claim_status, estimate = .pred_class),
  precision(evaluation_data, truth = claim_status, estimate = .pred_class, event_level = "second"),
  recall(evaluation_data, truth = claim_status, estimate = .pred_class, event_level = "second"),
  f_meas(evaluation_data, truth = claim_status, estimate = .pred_class, event_level = "second"),
  roc_auc(evaluation_data, truth = claim_status, .pred_yes, event_level = "second"),
  pr_auc(evaluation_data, truth = claim_status, .pred_yes, event_level = "second")
) %>%
  mutate(
    config_id = best_config_id,
    model = champion$model,
    strategy = champion$strategy,
    .before = 1
  )

confusion_matrix <- conf_mat(evaluation_data, truth = claim_status, estimate = .pred_class)
confusion_table <- as_tibble(confusion_matrix$table)

roc_data <- roc_curve(evaluation_data, truth = claim_status, .pred_yes, event_level = "second")
pr_data <- pr_curve(evaluation_data, truth = claim_status, .pred_yes, event_level = "second")

write_csv(final_metrics, here("reports", "final_metrics.csv"))
write_csv(confusion_table, here("reports", "confusion_matrix.csv"))
write_csv(roc_data, here("reports", "roc_curve_data.csv"))
write_csv(pr_data, here("reports", "precision_recall_curve_data.csv"))
saveRDS(evaluation_data, here("models", "test_predictions.rds"))
saveRDS(
  list(
    config_id = best_config_id,
    threshold = best_threshold,
    threshold_metrics = threshold_metrics
  ),
  here("models", "decision_threshold.rds")
)

p_confusion <- autoplot(confusion_matrix, type = "heatmap") +
  labs(title = "Matrice de confusion - modele champion") +
  theme_minimal()

p_roc <- autoplot(roc_data) +
  labs(title = "Courbe ROC - modele champion") +
  theme_minimal()

p_pr <- autoplot(pr_data) +
  labs(title = "Courbe precision/rappel - modele champion") +
  theme_minimal()

ggsave(here("reports", "figures", "confusion_matrix.png"), p_confusion, width = 7, height = 5)
ggsave(here("reports", "figures", "roc_curve.png"), p_roc, width = 7, height = 5)
ggsave(here("reports", "figures", "precision_recall_curve.png"), p_pr, width = 7, height = 5)

print(final_metrics)
print(confusion_matrix)

message("Evaluation finale sauvegardee : reports/final_metrics.csv")
message("Seuil de decision calibre hors test : ", best_threshold)
