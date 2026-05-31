# global.R
# Chargement global de l'application Shiny underwriting.

required_packages <- c("shiny", "bslib", "yaml", "dplyr", "tidymodels", "DT", "plotly", "purrr", "tibble")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Packages requis manquants : ", paste(missing_packages, collapse = ", "),
    "\nInstallez-les avec install.packages() avant de lancer l'application."
  )
}

library(shiny)
library(bslib)
library(yaml)
library(dplyr)
library(tidymodels)

find_project_root <- function() {
  candidates <- unique(normalizePath(c(".", ".."), winslash = "/", mustWork = FALSE))
  for (candidate in candidates) {
    if (file.exists(file.path(candidate, "models", "best_model.rds"))) {
      return(candidate)
    }
  }
  normalizePath("..", winslash = "/", mustWork = FALSE)
}

PROJECT_ROOT <- find_project_root()
Sys.setenv(TRAVEL_INSURANCE_PROJECT_ROOT = PROJECT_ROOT)

source(file.path(PROJECT_ROOT, "agent", "generate_recommendation.R"), local = TRUE)
source(file.path(PROJECT_ROOT, "agent", "explain_prediction.R"), local = TRUE)
source(file.path(PROJECT_ROOT, "agent", "build_agent_context.R"), local = TRUE)
source(file.path(PROJECT_ROOT, "agent", "call_llm_agent.R"), local = TRUE)

source(file.path(PROJECT_ROOT, "shiny_app", "modules", "mod_input_client.R"), local = TRUE)
source(file.path(PROJECT_ROOT, "shiny_app", "modules", "mod_score_output.R"), local = TRUE)
source(file.path(PROJECT_ROOT, "shiny_app", "modules", "mod_risk_explanation.R"), local = TRUE)
source(file.path(PROJECT_ROOT, "shiny_app", "modules", "mod_agent_chat.R"), local = TRUE)
source(file.path(PROJECT_ROOT, "shiny_app", "modules", "mod_client_scoring.R"), local = TRUE)
source(file.path(PROJECT_ROOT, "shiny_app", "modules", "mod_stat_exploration.R"), local = TRUE)
source(file.path(PROJECT_ROOT, "shiny_app", "modules", "mod_ml_summary.R"), local = TRUE)
source(file.path(PROJECT_ROOT, "shiny_app", "modules", "mod_pricing_simulation.R"), local = TRUE)

safe_read_rds <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  readRDS(path)
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

best_model <- safe_read_rds(file.path(PROJECT_ROOT, "models", "best_model.rds"))
model_metadata <- safe_read_rds(file.path(PROJECT_ROOT, "models", "model_metadata.rds"))
data_clean <- safe_read_rds(file.path(PROJECT_ROOT, "data", "processed", "data_clean.rds"))
train_data <- safe_read_rds(file.path(PROJECT_ROOT, "data", "processed", "train_data.rds"))
test_data <- safe_read_rds(file.path(PROJECT_ROOT, "data", "processed", "test_data.rds"))
model_comparison <- safe_read_csv(file.path(PROJECT_ROOT, "reports", "model_comparison.csv"))
model_ranking <- safe_read_csv(file.path(PROJECT_ROOT, "reports", "model_ranking.csv"))
final_metrics <- safe_read_csv(file.path(PROJECT_ROOT, "reports", "final_metrics.csv"))
confusion_matrix <- safe_read_csv(file.path(PROJECT_ROOT, "reports", "confusion_matrix.csv"))
class_balance_summary <- safe_read_csv(file.path(PROJECT_ROOT, "reports", "class_balance_summary.csv"))
risk_segments <- yaml::read_yaml(file.path(PROJECT_ROOT, "config", "risk_segments.yml"))
app_config <- yaml::read_yaml(file.path(PROJECT_ROOT, "config", "app_config.yml"))

if (is.null(best_model)) {
  warning("models/best_model.rds introuvable : l'application affichera un message d'erreur lors du scoring.")
}

get_choices <- function(variable, fallback) {
  if (!is.null(train_data) && variable %in% names(train_data)) {
    values <- sort(unique(stats::na.omit(as.character(train_data[[variable]]))))
    if (length(values) > 0) return(values)
  }
  fallback
}

input_choices <- list(
  agency = get_choices("agency", c("C2B", "CWT", "EPX", "JZI")),
  agency_type = get_choices("agency_type", c("Airlines", "Travel Agency")),
  distribution_channel = get_choices("distribution_channel", c("Online", "Offline")),
  product_name = get_choices("product_name", c("Cancellation Plan", "Basic Plan", "Comprehensive Plan")),
  destination = get_choices("destination", c("SINGAPORE", "MALAYSIA", "THAILAND", "CHINA", "AUSTRALIA")),
  gender = get_choices("gender", c("F", "M"))
)

get_operational_threshold <- function() {
  if (!is.null(model_metadata) && !is.null(model_metadata$threshold_analysis$business_threshold$threshold)) {
    return(as.numeric(model_metadata$threshold_analysis$business_threshold$threshold))
  }
  if (!is.null(model_metadata) && !is.null(model_metadata$decision_threshold$threshold)) {
    return(as.numeric(model_metadata$decision_threshold$threshold))
  }
  if (!is.null(app_config$app$default_threshold)) {
    return(as.numeric(app_config$app$default_threshold))
  }
  as.numeric(app_config$app$fallback_threshold %||% 0.08)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

format_pct <- function(x, digits = 1) {
  paste0(round(100 * as.numeric(x), digits), " %")
}

assign_risk_segment <- function(probability_yes) {
  ordered_keys <- c("very_high", "high", "moderate_high", "moderate", "standard")
  for (key in ordered_keys) {
    segment <- risk_segments[[key]]
    if (probability_yes >= as.numeric(segment$min_score)) {
      return(list(
        key = key,
        label = segment$label,
        condition = segment$condition,
        claim_rate = as.numeric(segment$claim_rate),
        lift = as.numeric(segment$lift),
        recommendation = segment$recommendation
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
    recommendation = segment$recommendation
  )
}

predict_claim_risk <- function(client_data) {
  if (is.null(best_model)) {
    stop("Le modèle champion n'est pas chargé. Vérifiez models/best_model.rds.")
  }

  required_cols <- c(
    "agency", "agency_type", "distribution_channel", "product_name",
    "duration", "destination", "net_sales", "commision_in_value",
    "gender", "age"
  )
  missing_cols <- setdiff(required_cols, names(client_data))
  if (length(missing_cols) > 0) {
    stop("Colonnes manquantes pour la prédiction : ", paste(missing_cols, collapse = ", "))
  }

  client_data <- client_data[, required_cols, drop = FALSE]
  probability_yes <- as.numeric(predict(best_model, new_data = client_data, type = "prob")$.pred_yes)
  threshold_used <- get_operational_threshold()
  predicted_class <- ifelse(probability_yes >= threshold_used, "yes", "no")
  segment <- assign_risk_segment(probability_yes)
  recommendation <- generate_recommendation(
    probability = probability_yes,
    risk_segment = segment$label,
    lift = segment$lift,
    claim_rate = segment$claim_rate
  )

  list(
    probability_yes = probability_yes,
    predicted_class = predicted_class,
    threshold_used = threshold_used,
    risk_segment = segment$label,
    risk_segment_key = segment$key,
    segment_condition = segment$condition,
    segment_claim_rate = segment$claim_rate,
    segment_lift = segment$lift,
    recommendation_level = recommendation,
    configured_recommendation = segment$recommendation,
    baseline_claim_rate = as.numeric(app_config$app$baseline_claim_rate %||% 0.0168),
    score_threshold_source = app_config$app$score_threshold_source
  )
}
