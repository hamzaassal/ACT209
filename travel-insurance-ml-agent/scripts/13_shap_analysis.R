# 13_shap_analysis.R
# Analyse SHAP du modele champion XGBoost sans reentrainement.

required_packages <- c("workflows", "recipes", "xgboost", "dplyr", "readr", "ggplot2", "tidyr", "here")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    "Packages manquants : ", paste(missing_packages, collapse = ", "),
    ". Installe-les avec install.packages(...) puis relance ce script."
  )
}

library(workflows)
library(recipes)
library(xgboost)
library(dplyr)
library(readr)
library(ggplot2)
library(tidyr)
library(here)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || anyNA(x)) y else x
}

dir.create(here("reports"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("reports", "figures"), showWarnings = FALSE, recursive = TRUE)

best_model <- readRDS(here("models", "best_model.rds"))
test_data <- readRDS(here("data", "processed", "test_data.rds"))

prettify_feature <- function(feature) {
  dplyr::case_when(
    grepl("^agency_", feature) ~ "Agence",
    grepl("^agency_type_", feature) ~ "Type d'agence",
    grepl("^distribution_channel_", feature) ~ "Canal de distribution",
    grepl("^product_name_", feature) ~ "Produit",
    grepl("^destination_", feature) ~ "Destination",
    grepl("^gender_", feature) ~ "Genre",
    feature == "duration" ~ "Durée du voyage",
    feature == "net_sales" ~ "Net sales",
    feature == "commision_in_value" ~ "Commission",
    feature == "age" ~ "Âge",
    TRUE ~ feature
  )
}

recipe_fit <- workflows::extract_recipe(best_model, estimated = TRUE)
booster <- workflows::extract_fit_parsnip(best_model)$fit

baked_test <- recipes::bake(recipe_fit, new_data = test_data) %>%
  dplyr::select(-claim_status)

feature_names <- booster$feature_names %||% colnames(baked_test)
missing_features <- setdiff(feature_names, colnames(baked_test))
if (length(missing_features) > 0) {
  for (feature in missing_features) baked_test[[feature]] <- 0
}
baked_test <- baked_test[, feature_names, drop = FALSE]

dmatrix <- xgboost::xgb.DMatrix(data = as.matrix(baked_test))
shap_matrix <- predict(booster, dmatrix, predcontrib = TRUE)
shap_df <- as.data.frame(shap_matrix, check.names = FALSE)
bias_cols <- grep("^bias$|^BIAS$|^\\(Intercept\\)$", names(shap_df), ignore.case = TRUE, value = TRUE)
feature_shap_df <- shap_df %>% dplyr::select(-dplyr::any_of(bias_cols))

global_importance <- shap_df %>%
  dplyr::select(-dplyr::any_of(bias_cols)) %>%
  summarise(across(everything(), ~ mean(abs(.x), na.rm = TRUE))) %>%
  tidyr::pivot_longer(everything(), names_to = "feature", values_to = "mean_abs_shap") %>%
  mutate(feature_group = prettify_feature(feature)) %>%
  arrange(desc(mean_abs_shap))

grouped_importance <- global_importance %>%
  group_by(feature_group) %>%
  summarise(mean_abs_shap = sum(mean_abs_shap), .groups = "drop") %>%
  arrange(desc(mean_abs_shap))

readr::write_csv(global_importance, here("reports", "shap_global_importance.csv"))
readr::write_csv(grouped_importance, here("reports", "shap_grouped_importance.csv"))

top_grouped <- grouped_importance %>% slice_head(n = 10)

ggplot(top_grouped, aes(x = reorder(feature_group, mean_abs_shap), y = mean_abs_shap)) +
  geom_col(fill = "#234f7d", width = 0.72) +
  coord_flip() +
  labs(
    title = "Importance globale SHAP - modèle champion",
    x = NULL,
    y = "Contribution absolue moyenne"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  here("reports", "figures", "shap_global_importance.png"),
  width = 8,
  height = 5,
  dpi = 150
)

# Quelques contributions locales sont sauvegardées pour contrôle, sans inventer
# d'interprétation individuelle hors du dossier réellement scoré dans Shiny.
local_examples <- shap_df %>%
  mutate(row_id = row_number()) %>%
  select(row_id, everything()) %>%
  slice_head(n = 20)

readr::write_csv(local_examples, here("reports", "shap_test_contributions_sample.csv"))

message("Analyse SHAP terminée.")
message("Fichiers générés : reports/shap_global_importance.csv, reports/shap_grouped_importance.csv, reports/figures/shap_global_importance.png")
