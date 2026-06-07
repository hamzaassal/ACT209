# 15_refresh_final_smote_summary.R
# Rafraichit la synthese finale pour aligner le rapport avec le modele complet
# actuellement sauvegarde : XGBoost avec SMOTE.

required_packages <- c("dplyr", "readr", "here")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Packages manquants : ", paste(missing_packages, collapse = ", "))
}

library(dplyr)
library(readr)
library(here)

metadata_path <- here("models", "model_metadata.rds")
if (!file.exists(metadata_path)) {
  stop("Metadonnees du modele introuvables : ", metadata_path)
}

metadata <- readRDS(metadata_path)
champion <- metadata$champion
final_metrics <- metadata$final_metrics

metric_value <- function(metric_name) {
  final_metrics %>%
    filter(.metric == metric_name) %>%
    pull(.estimate) %>%
    first()
}

lift_table <- read_csv(here("reports", "risk_ranking_lift_table.csv"), show_col_types = FALSE)
lift_top_10 <- lift_table %>% filter(segment == "top_10_percent") %>% pull(lift) %>% first()
claims_top_10 <- lift_table %>% filter(segment == "top_10_percent") %>% pull(share_of_total_claims_captured) %>% first()
lift_top_20 <- lift_table %>% filter(segment == "top_20_percent") %>% pull(lift) %>% first()
claims_top_20 <- lift_table %>% filter(segment == "top_20_percent") %>% pull(share_of_total_claims_captured) %>% first()

summary_table <- tibble(
  candidate_model = champion$model,
  strategy = champion$strategy,
  tuned_or_not = FALSE,
  roc_auc = metric_value("roc_auc"),
  pr_auc = metric_value("pr_auc"),
  f1 = metric_value("f_meas"),
  recall = metric_value("recall"),
  precision = metric_value("precision"),
  lift_top_10 = lift_top_10,
  claims_captured_top_10 = claims_top_10,
  lift_top_20 = lift_top_20,
  claims_captured_top_20 = claims_top_20,
  final_rank = 1,
  recommendation = "Champion statistique du modele complet : XGBoost avec SMOTE."
)

write_csv(summary_table, here("reports", "final_model_selection_summary.csv"))
print(summary_table)
