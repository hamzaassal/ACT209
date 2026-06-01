# 16_refresh_current_champion_lift.R
# Recalcule l'analyse lift/ranking avec le modele actuellement sauvegarde.

required_packages <- c("tidymodels", "dplyr", "readr", "here")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Packages manquants : ", paste(missing_packages, collapse = ", "))
}

library(tidymodels)
library(dplyr)
library(readr)
library(here)

model_path <- here("models", "best_model.rds")
metadata_path <- here("models", "model_metadata.rds")
test_path <- here("data", "processed", "test_data.rds")

if (!file.exists(model_path) || !file.exists(metadata_path) || !file.exists(test_path)) {
  stop("Modele, metadonnees ou test set introuvables.")
}

model <- readRDS(model_path)
metadata <- readRDS(metadata_path)
test_data <- readRDS(test_path)

prob_yes <- predict(model, test_data, type = "prob")$.pred_yes
scored <- test_data %>%
  transmute(
    claim_status = claim_status,
    probability_yes = prob_yes,
    claim_flag = as.integer(claim_status == "yes")
  ) %>%
  arrange(desc(probability_yes))

total_claims <- sum(scored$claim_flag)
baseline_claim_rate <- mean(scored$claim_flag)
total_n <- nrow(scored)

lift_table <- purrr::map_dfr(c(0.01, 0.05, 0.10, 0.20, 0.30), function(segment_share) {
  n_segment <- ceiling(total_n * segment_share)
  segment_data <- scored %>% slice_head(n = n_segment)
  n_claims <- sum(segment_data$claim_flag)
  claim_rate <- n_claims / n_segment
  tibble(
    segment = paste0("top_", round(segment_share * 100), "_percent"),
    n_observations = n_segment,
    n_claims_captured = n_claims,
    share_of_total_claims_captured = n_claims / total_claims,
    claim_rate_in_segment = claim_rate,
    baseline_claim_rate = baseline_claim_rate,
    lift = claim_rate / baseline_claim_rate,
    config_id = metadata$champion$config_id,
    model = metadata$champion$model,
    strategy = metadata$champion$strategy
  )
})

write_csv(lift_table, here("reports", "risk_ranking_lift_table.csv"))

final_summary_path <- here("reports", "final_model_selection_summary.csv")
if (file.exists(final_summary_path)) {
  final_summary <- read_csv(final_summary_path, show_col_types = FALSE)
  final_summary <- final_summary %>%
    mutate(
      lift_top_10 = lift_table %>% filter(segment == "top_10_percent") %>% pull(lift) %>% first(),
      claims_captured_top_10 = lift_table %>% filter(segment == "top_10_percent") %>% pull(share_of_total_claims_captured) %>% first(),
      lift_top_20 = lift_table %>% filter(segment == "top_20_percent") %>% pull(lift) %>% first(),
      claims_captured_top_20 = lift_table %>% filter(segment == "top_20_percent") %>% pull(share_of_total_claims_captured) %>% first()
    )
  write_csv(final_summary, final_summary_path)
}

print(lift_table)
