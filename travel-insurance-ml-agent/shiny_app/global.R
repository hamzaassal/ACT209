# global.R
# Chargement global de l'application Shiny underwriting.

required_packages <- c("shiny", "bslib", "yaml", "dplyr", "tidymodels", "DT", "plotly", "purrr", "tibble")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Packages requis manquants : ", paste(missing_packages, collapse = ", "),
    "\nInstallez-les avec install.packages(...) avant de lancer l'application."
  )
}

library(shiny)
library(bslib)
library(yaml)
library(dplyr)
library(tidymodels)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || anyNA(x)) y else x
}

find_project_root <- function() {
  candidates <- unique(normalizePath(c(".", "..", "travel-insurance-ml-agent"), winslash = "/", mustWork = FALSE))
  for (candidate in candidates) {
    if (
      file.exists(file.path(candidate, "models", "no_tariff_best_model.rds")) ||
        file.exists(file.path(candidate, "models", "best_model.rds"))
    ) {
      return(candidate)
    }
  }
  normalizePath("..", winslash = "/", mustWork = FALSE)
}

PROJECT_ROOT <- find_project_root()
Sys.setenv(TRAVEL_INSURANCE_PROJECT_ROOT = PROJECT_ROOT)

# Charge explicitement le fichier .Renviron du projet au démarrage de Shiny.
# Cela évite de dépendre d'un redémarrage complet de RStudio après ajout de la clé API.
project_renviron <- file.path(PROJECT_ROOT, ".Renviron")
if (file.exists(project_renviron)) {
  readRenviron(project_renviron)
}

safe_read_rds <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(readRDS(path), error = function(e) NULL)
}

safe_read_csv_base <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
}

www_dir <- file.path(PROJECT_ROOT, "shiny_app", "www")
assets_logo <- file.path(PROJECT_ROOT, "assets", "logo-cnam.png")
www_logo <- file.path(www_dir, "logo-cnam.png")
www_logo_trimmed <- file.path(www_dir, "logo-cnam-trimmed.png")
if (file.exists(assets_logo) && !file.exists(www_logo)) {
  dir.create(www_dir, showWarnings = FALSE, recursive = TRUE)
  try(file.copy(assets_logo, www_logo, overwrite = FALSE), silent = TRUE)
}

if (dir.exists(file.path(PROJECT_ROOT, "docs"))) {
  addResourcePath("docs", file.path(PROJECT_ROOT, "docs"))
}
if (dir.exists(file.path(PROJECT_ROOT, "reports", "figures"))) {
  addResourcePath("report_figures", file.path(PROJECT_ROOT, "reports", "figures"))
}

source(file.path(PROJECT_ROOT, "agent", "generate_recommendation.R"), local = TRUE)
source(file.path(PROJECT_ROOT, "agent", "explain_prediction.R"), local = TRUE)
source(file.path(PROJECT_ROOT, "agent", "build_agent_context.R"), local = TRUE)
source(file.path(PROJECT_ROOT, "agent", "call_llm_agent.R"), local = TRUE)

source(file.path(PROJECT_ROOT, "shiny_app", "modules", "mod_input_client.R"), local = TRUE)
source(file.path(PROJECT_ROOT, "shiny_app", "modules", "mod_score_output.R"), local = TRUE)
source(file.path(PROJECT_ROOT, "shiny_app", "modules", "mod_risk_analysis.R"), local = TRUE)
source(file.path(PROJECT_ROOT, "shiny_app", "modules", "mod_agent_chat.R"), local = TRUE)
source(file.path(PROJECT_ROOT, "shiny_app", "modules", "mod_client_scoring.R"), local = TRUE)
source(file.path(PROJECT_ROOT, "shiny_app", "modules", "mod_stat_exploration.R"), local = TRUE)
source(file.path(PROJECT_ROOT, "shiny_app", "modules", "mod_ml_summary.R"), local = TRUE)
source(file.path(PROJECT_ROOT, "shiny_app", "modules", "mod_pricing_simulation.R"), local = TRUE)

app_model_path <- file.path(PROJECT_ROOT, "models", "no_tariff_best_model.rds")
app_metadata_path <- file.path(PROJECT_ROOT, "models", "no_tariff_model_metadata.rds")
app_model_source <- "Modèle applicatif sans variable tarifaire"

if (!file.exists(app_model_path)) {
  app_model_path <- file.path(PROJECT_ROOT, "models", "best_model.rds")
  app_metadata_path <- file.path(PROJECT_ROOT, "models", "model_metadata.rds")
  app_model_source <- "Modèle complet historique"
}

safe_read_csv <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

round_numeric <- function(data, digits = 4) {
  data %>%
    mutate(across(where(is.numeric), ~ round(.x, digits)))
}

data_clean <- safe_read_rds(file.path(PROJECT_ROOT, "data", "processed", "data_clean.rds"))
best_model <- safe_read_rds(app_model_path)
model_metadata <- safe_read_rds(app_metadata_path)
train_data <- safe_read_rds(file.path(PROJECT_ROOT, "data", "processed", "train_data.rds"))
test_data <- safe_read_rds(file.path(PROJECT_ROOT, "data", "processed", "test_data.rds"))
model_comparison <- safe_read_csv(file.path(PROJECT_ROOT, "reports", "model_comparison.csv"))
model_ranking <- safe_read_csv(file.path(PROJECT_ROOT, "reports", "model_ranking.csv"))
final_metrics <- safe_read_csv(file.path(PROJECT_ROOT, "reports", "final_metrics.csv"))
confusion_matrix <- safe_read_csv(file.path(PROJECT_ROOT, "reports", "confusion_matrix.csv"))
class_balance_summary <- safe_read_csv(file.path(PROJECT_ROOT, "reports", "class_balance_summary.csv"))
risk_segments <- yaml::read_yaml(file.path(PROJECT_ROOT, "config", "risk_segments.yml"))
app_config <- yaml::read_yaml(file.path(PROJECT_ROOT, "config", "app_config.yml"))
lift_table <- safe_read_csv_base(file.path(PROJECT_ROOT, "reports", "risk_ranking_lift_table.csv"))
final_selection <- safe_read_csv_base(file.path(PROJECT_ROOT, "reports", "final_model_selection_summary.csv"))
shap_grouped_importance <- safe_read_csv_base(file.path(PROJECT_ROOT, "reports", "shap_grouped_importance.csv"))

model_is_loaded <- !is.null(best_model)
logo_available <- file.exists(www_logo) || file.exists(www_logo_trimmed)

if (!model_is_loaded) {
  warning("Modele applicatif introuvable : l'application affichera un message d'erreur lors du scoring.")
}

get_choices <- function(variable, fallback) {
  if (!is.null(train_data) && variable %in% names(train_data)) {
    values <- sort(unique(stats::na.omit(as.character(train_data[[variable]]))))
    if (length(values) > 0) return(values)
  }
  fallback
}

select_existing <- function(choices, preferred) {
  if (preferred %in% choices) preferred else choices[[1]]
}

input_choices <- list(
  agency = get_choices("agency", c("C2B", "CWT", "EPX", "JZI")),
  agency_type = get_choices("agency_type", c("Airlines", "Travel Agency")),
  distribution_channel = get_choices("distribution_channel", c("Online", "Offline")),
  product_name = get_choices("product_name", c("Cancellation Plan", "Basic Plan", "Comprehensive Plan")),
  destination = get_choices("destination", c("SINGAPORE", "MALAYSIA", "THAILAND", "CHINA", "AUSTRALIA")),
  gender = get_choices("gender", c("F", "M"))
)

format_pct <- function(x, digits = 1) {
  paste0(round(100 * as.numeric(x), digits), " %")
}

format_probability <- function(x) {
  paste0(format(round(100 * as.numeric(x), 2), nsmall = 2), " %")
}

get_operational_threshold <- function() {
  no_tariff_threshold <- tryCatch(model_metadata$threshold[[1]], error = function(e) NA_real_)
  if (!is.na(no_tariff_threshold)) return(as.numeric(no_tariff_threshold))

  business_threshold <- tryCatch(
    model_metadata$threshold_analysis$business_threshold$threshold[[1]],
    error = function(e) NA_real_
  )
  if (!is.na(business_threshold)) return(as.numeric(business_threshold))

  app_threshold <- app_config$app$default_threshold %||% NA_real_
  if (!is.na(as.numeric(app_threshold))) return(as.numeric(app_threshold))

  decision_threshold <- tryCatch(
    model_metadata$decision_threshold$threshold[[1]],
    error = function(e) NA_real_
  )
  if (!is.na(decision_threshold)) return(as.numeric(decision_threshold))

  as.numeric(app_config$app$fallback_threshold %||% 0.08)
}

get_model_label <- function() {
  if (!is.null(model_metadata$approach) && identical(model_metadata$approach, "no_tariff")) {
    champion <- model_metadata$champion
    return(paste0(
      "XGBoost sans SMOTE - approche underwriting sans variable tarifaire",
      " (seuil ", format_probability(model_metadata$threshold %||% 0.07), ")"
    ))
  }

  if (!is.null(final_selection) && nrow(final_selection) > 0) {
    return(paste(final_selection$candidate_model[[1]], final_selection$strategy[[1]], sep = " - "))
  }
  app_config$app$champion_model %||% "XGBoost sans SMOTE"
}

logo_block <- function(compact = FALSE) {
  if (logo_available) {
    logo_src <- if (file.exists(www_logo_trimmed)) "logo-cnam-trimmed.png" else "logo-cnam.png"
    tags$img(
      src = logo_src,
      alt = "Logo université",
      class = if (compact) "brand-logo compact" else "brand-logo"
    )
  } else {
    tags$div(class = "logo-fallback", "Université")
  }
}

risk_css_class <- function(segment_key) {
  switch(
    segment_key,
    very_high = "risk-very-high",
    high = "risk-high",
    moderate_high = "risk-moderate-high",
    moderate = "risk-moderate",
    moderate_low = "risk-moderate-low",
    "risk-standard"
  )
}

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

compute_local_shap <- function(client_data, n = 6) {
  if (is.null(best_model) || !requireNamespace("xgboost", quietly = TRUE)) return(NULL)

  tryCatch({
    recipe_fit <- workflows::extract_recipe(best_model, estimated = TRUE)
    booster <- workflows::extract_fit_parsnip(best_model)$fit
    baked <- recipes::bake(recipe_fit, new_data = client_data)
    baked <- baked[, setdiff(names(baked), "claim_status"), drop = FALSE]

    feature_names <- booster$feature_names %||% colnames(baked)
    missing_features <- setdiff(feature_names, colnames(baked))
    if (length(missing_features) > 0) {
      for (feature in missing_features) baked[[feature]] <- 0
    }
    baked <- baked[, feature_names, drop = FALSE]

    shap_matrix <- predict(
      booster,
      xgboost::xgb.DMatrix(data = as.matrix(baked)),
      predcontrib = TRUE
    )
    shap_df <- as.data.frame(shap_matrix, check.names = FALSE)
    bias_cols <- grep("^bias$|^BIAS$|^\\(Intercept\\)$", names(shap_df), ignore.case = TRUE, value = TRUE)

    shap_df %>%
      dplyr::select(-dplyr::any_of(bias_cols)) %>%
      tidyr::pivot_longer(everything(), names_to = "feature", values_to = "shap_value") %>%
      dplyr::mutate(
        feature_group = prettify_feature(feature),
        abs_shap = abs(shap_value)
      ) %>%
      dplyr::group_by(feature_group) %>%
      dplyr::summarise(
        shap_value = sum(shap_value),
        abs_shap = sum(abs_shap),
        .groups = "drop"
      ) %>%
      dplyr::mutate(direction = ifelse(shap_value >= 0, "Augmente le score", "Diminue le score")) %>%
      dplyr::arrange(desc(abs_shap)) %>%
      dplyr::slice_head(n = n)
  }, error = function(e) NULL)
}

assign_risk_segment <- function(probability_yes) {
  ordered_keys <- c("very_high", "high", "moderate_high", "moderate", "moderate_low", "standard")
  for (key in ordered_keys) {
    segment <- risk_segments[[key]]
    if (!is.null(segment) && probability_yes >= as.numeric(segment$min_score)) {
      return(list(
        key = key,
        label = segment$label,
        condition = segment$condition,
        claim_rate = as.numeric(segment$claim_rate),
        lift = as.numeric(segment$lift),
        recommendation = segment$recommendation,
        min_score = as.numeric(segment$min_score)
      ))
    }
  }
  segment <- risk_segments$standard
  list(
    key = "standard",
    label = segment$label,
    condition = segment$condition,
    claim_rate = as.numeric(segment$claim_rate),
    lift = as.numeric(segment$lift),
    recommendation = segment$recommendation,
    min_score = as.numeric(segment$min_score)
  )
}

validate_client_data <- function(client_data) {
  required_cols <- c(
    "agency", "agency_type", "distribution_channel", "product_name",
    "duration", "destination", "gender", "age"
  )
  missing_cols <- setdiff(required_cols, names(client_data))
  if (length(missing_cols) > 0) {
    stop("Colonnes manquantes pour la prédiction : ", paste(missing_cols, collapse = ", "))
  }

  numeric_cols <- c("duration", "age")
  for (col in numeric_cols) {
    if (is.na(client_data[[col]][[1]]) || !is.finite(as.numeric(client_data[[col]][[1]]))) {
      stop("Valeur numérique invalide pour le champ : ", col)
    }
  }

  client_data[, required_cols, drop = FALSE]
}

predict_claim_risk <- function(client_data) {
  if (is.null(best_model)) {
    stop("Le modele applicatif n'est pas charge. Verifiez models/no_tariff_best_model.rds ou models/best_model.rds.")
  }

  client_data <- validate_client_data(client_data)
  probability_yes <- tryCatch(
    as.numeric(predict(best_model, new_data = client_data, type = "prob")$.pred_yes),
    error = function(e) {
      stop("La prédiction a échoué : ", conditionMessage(e))
    }
  )

  threshold_used <- get_operational_threshold()
  predicted_class <- ifelse(probability_yes >= threshold_used, "yes", "no")
  segment <- assign_risk_segment(probability_yes)
  recommendation <- generate_recommendation(
    probability = probability_yes,
    risk_segment = segment$label,
    lift = segment$lift,
    claim_rate = segment$claim_rate
  )
  shap_top_features <- compute_local_shap(client_data)

  list(
    probability_yes = probability_yes,
    predicted_class = predicted_class,
    threshold_used = threshold_used,
    risk_segment = segment$label,
    risk_level = segment$label,
    risk_segment_key = segment$key,
    risk_css_class = risk_css_class(segment$key),
    segment_condition = segment$condition,
    segment_claim_rate = segment$claim_rate,
    segment_lift = segment$lift,
    segment_min_score = segment$min_score,
    recommendation = recommendation,
    recommendation_level = recommendation,
    configured_recommendation = segment$recommendation,
    baseline_claim_rate = as.numeric(app_config$app$baseline_claim_rate %||% 0.0168),
    score_threshold_source = app_config$app$score_threshold_source,
    model_label = get_model_label(),
    model_source = app_model_source,
    removed_tariff_variables = model_metadata$removed_variables %||% character(0),
    shap_top_features = shap_top_features
  )
}
