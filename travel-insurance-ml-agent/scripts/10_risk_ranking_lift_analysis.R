# 10_risk_ranking_lift_analysis.R
# Analyse de scoring/lift du modele champion XGBoost sans SMOTE.
# Cette analyse complete la classification binaire : elle mesure la capacite du
# score a concentrer les sinistres dans les dossiers les plus risques.

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

test_path <- here("data", "processed", "test_data.rds")
model_path <- here("models", "xgboost__none_fit.rds")

if (!file.exists(test_path)) {
  stop("Test set introuvable : data/processed/test_data.rds")
}

if (!file.exists(model_path)) {
  stop("Modele XGBoost sans SMOTE introuvable : models/xgboost__none_fit.rds")
}

test_data <- readRDS(test_path)
xgb_none <- readRDS(model_path)

prob_pred <- predict(xgb_none, new_data = test_data, type = "prob")

if (!".pred_yes" %in% names(prob_pred)) {
  stop("Colonne .pred_yes introuvable dans les predictions.")
}

scored_test <- bind_cols(
  test_data %>% select(claim_status),
  prob_pred %>% select(.pred_yes)
) %>%
  arrange(desc(.pred_yes)) %>%
  mutate(
    row_rank = row_number(),
    claim_flag = as.integer(claim_status == "yes"),
    cumulative_claims = cumsum(claim_flag),
    cumulative_observations = row_number(),
    cumulative_population_share = cumulative_observations / n(),
    cumulative_claim_share = cumulative_claims / sum(claim_flag)
  )

total_observations <- nrow(scored_test)
total_claims <- sum(scored_test$claim_flag)
baseline_claim_rate <- total_claims / total_observations

segments <- tibble(
  segment = c("top_1_percent", "top_5_percent", "top_10_percent", "top_20_percent", "top_30_percent"),
  population_share = c(0.01, 0.05, 0.10, 0.20, 0.30)
)

risk_ranking_lift_table <- segments %>%
  rowwise() %>%
  mutate(
    n_observations = max(1, ceiling(total_observations * population_share)),
    n_claims_captured = sum(scored_test$claim_flag[seq_len(n_observations)]),
    share_of_total_claims_captured = n_claims_captured / total_claims,
    claim_rate_in_segment = n_claims_captured / n_observations,
    baseline_claim_rate = baseline_claim_rate,
    lift = claim_rate_in_segment / baseline_claim_rate
  ) %>%
  ungroup() %>%
  select(
    segment,
    n_observations,
    n_claims_captured,
    share_of_total_claims_captured,
    claim_rate_in_segment,
    baseline_claim_rate,
    lift
  )

write_csv(risk_ranking_lift_table, here("reports", "risk_ranking_lift_table.csv"))
write_csv(scored_test, here("reports", "scored_test_xgboost_none.csv"))

p_lift <- risk_ranking_lift_table %>%
  mutate(segment = factor(segment, levels = segment)) %>%
  ggplot(aes(x = segment, y = lift, group = 1)) +
  geom_line(color = "#2b6cb0", linewidth = 1) +
  geom_point(color = "#2b6cb0", size = 2.5) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  labs(title = "Courbe de lift par segment de score", x = "Segment", y = "Lift") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, hjust = 1))

p_gains <- scored_test %>%
  ggplot(aes(x = cumulative_population_share, y = cumulative_claim_share)) +
  geom_line(color = "#c53030", linewidth = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Cumulative gains chart",
    x = "Part cumulée de la population triée par score décroissant",
    y = "Part cumulée des sinistres capturés"
  ) +
  theme_minimal()

p_score_distribution <- scored_test %>%
  ggplot(aes(x = .pred_yes, fill = claim_status)) +
  geom_density(alpha = 0.45) +
  scale_fill_manual(values = c(no = "#2b6cb0", yes = "#c53030")) +
  labs(
    title = "Distribution des scores par classe réelle",
    x = "Probabilité prédite de sinistre",
    y = "Densité",
    fill = "Sinistre"
  ) +
  theme_minimal()

ggsave(here("reports", "figures", "risk_ranking_lift_curve.png"), p_lift, width = 8, height = 5)
ggsave(here("reports", "figures", "risk_ranking_cumulative_gains.png"), p_gains, width = 8, height = 5)
ggsave(here("reports", "figures", "risk_score_distribution_by_class.png"), p_score_distribution, width = 8, height = 5)

print(risk_ranking_lift_table)
message("Analyse de scoring/lift sauvegardee : reports/risk_ranking_lift_table.csv")
