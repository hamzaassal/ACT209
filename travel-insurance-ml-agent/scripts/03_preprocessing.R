# 03_preprocessing.R
# Split train/test et recettes de preprocessing.
# SMOTE n'est pas applique ici : il sera applique uniquement dans les folds de
# validation croisee via une recette dediee dans le script de modelisation.

required_packages <- c("dplyr", "rsample", "recipes", "here")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Packages manquants : ", paste(missing_packages, collapse = ", "))
}

library(dplyr)
library(rsample)
library(recipes)
library(here)

set.seed(123)

dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)

data_path <- here("data", "processed", "data_clean.rds")
if (!file.exists(data_path)) {
  stop("Fichier introuvable : data/processed/data_clean.rds. Executez scripts/01_import_data.R.")
}

data_clean <- readRDS(data_path)

if (!"claim_status" %in% names(data_clean)) {
  stop("Variable cible introuvable : claim_status")
}

data_model <- data_clean %>%
  mutate(
    claim_status = case_when(
      claim_status %in% c(1, "1", "Yes", "yes", "YES", TRUE) ~ "yes",
      claim_status %in% c(0, "0", "No", "no", "NO", FALSE) ~ "no",
      TRUE ~ tolower(as.character(claim_status))
    ),
    claim_status = factor(claim_status, levels = c("no", "yes"))
  )

if (any(is.na(data_model$claim_status)) || length(levels(data_model$claim_status)) != 2) {
  stop("La cible claim_status doit etre un facteur binaire avec niveaux no / yes.")
}

split_obj <- initial_split(data_model, prop = 0.80, strata = claim_status)
train_data <- training(split_obj)
test_data <- testing(split_obj)

recipe_base <- recipe(claim_status ~ ., data = train_data) %>%
  step_rm(any_of(c("index", "x", "x1", "unnamed_0"))) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_impute_mode(all_nominal_predictors()) %>%
  # Regroupement des modalites rares avant encodage pour limiter la dimension
  # et stabiliser les folds, notamment pour destination et product_name.
  step_other(all_nominal_predictors(), threshold = 0.005, other = "other") %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

saveRDS(train_data, here("data", "processed", "train_data.rds"))
saveRDS(test_data, here("data", "processed", "test_data.rds"))
saveRDS(recipe_base, here("data", "processed", "recipe_base.rds"))

message("Preprocessing sauvegarde : train_data.rds, test_data.rds, recipe_base.rds.")
