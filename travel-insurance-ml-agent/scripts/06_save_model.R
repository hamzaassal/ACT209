# 06_save_model.R
# Sauvegarde du modele champion et des metadonnees utiles a Shiny/rapport.

required_packages <- c("dplyr", "readr", "here")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Packages manquants : ", paste(missing_packages, collapse = ", "))
}

library(dplyr)
library(readr)
library(here)

dir.create(here("models"), recursive = TRUE, showWarnings = FALSE)

ranking_path <- here("reports", "model_ranking.csv")
comparison_path <- here("reports", "model_comparison.csv")
thresholds_path <- here("reports", "model_thresholds.csv")
metrics_path <- here("reports", "final_metrics.csv")
threshold_path <- here("models", "decision_threshold.rds")
recipe_path <- here("data", "processed", "recipe_base.rds")

if (!file.exists(ranking_path)) {
  stop("Classement absent : executez scripts/04_model_training.R.")
}

model_ranking <- read_csv(ranking_path, show_col_types = FALSE)
model_comparison <- if (file.exists(comparison_path)) read_csv(comparison_path, show_col_types = FALSE) else NULL
model_thresholds <- if (file.exists(thresholds_path)) read_csv(thresholds_path, show_col_types = FALSE) else NULL
final_metrics <- if (file.exists(metrics_path)) read_csv(metrics_path, show_col_types = FALSE) else NULL
decision_threshold <- if (file.exists(threshold_path)) readRDS(threshold_path) else NULL
recipe_base <- if (file.exists(recipe_path)) readRDS(recipe_path) else NULL

champion <- model_ranking %>% arrange(desc(roc_auc), desc(f_meas), desc(recall)) %>% slice(1)
best_config_id <- champion$config_id
best_model_source <- here("models", paste0(best_config_id, "_fit.rds"))

if (!file.exists(best_model_source)) {
  stop("Modele champion introuvable : ", best_model_source)
}

best_model <- readRDS(best_model_source)

model_metadata <- list(
  task = "Prediction de la survenance d'un sinistre travel insurance",
  target = "claim_status",
  positive_class = "yes",
  negative_class = "no",
  champion = champion,
  selection_rule = "ROC AUC, puis F1-score, puis recall sur la classe yes",
  model_comparison = model_comparison,
  model_thresholds = model_thresholds,
  final_metrics = final_metrics,
  decision_threshold = decision_threshold,
  recipe_base = recipe_base,
  created_at = Sys.time()
)

saveRDS(best_model, here("models", "best_model.rds"))
saveRDS(model_metadata, here("models", "model_metadata.rds"))

message("Modele champion sauvegarde : models/best_model.rds")
message("Metadonnees sauvegardees : models/model_metadata.rds")
