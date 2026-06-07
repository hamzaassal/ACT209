# 08_test_shiny_agent_components.R
# Test léger des composants Shiny et agent IA sans lancer l'interface.

required_packages <- c("shiny", "bslib", "yaml", "dplyr", "tidymodels")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Packages requis manquants : ", paste(missing_packages, collapse = ", "),
    "\nInstallez-les avec install.packages(...) puis relancez le test."
  )
}

project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
source(file.path(project_root, "shiny_app", "global.R"), local = FALSE)

dir.create(file.path(project_root, "reports"), showWarnings = FALSE, recursive = TRUE)

client_example <- data.frame(
  agency = select_existing(input_choices$agency, "C2B"),
  agency_type = select_existing(input_choices$agency_type, "Airlines"),
  distribution_channel = select_existing(input_choices$distribution_channel, "Online"),
  product_name = select_existing(input_choices$product_name, "Cancellation Plan"),
  duration = 15,
  destination = select_existing(input_choices$destination, "SINGAPORE"),
  gender = select_existing(input_choices$gender, "F"),
  age = 35,
  stringsAsFactors = FALSE
)

prediction_result <- predict_claim_risk(client_example)
context_text <- build_agent_context(client_example, prediction_result)
fallback_answer <- call_llm_agent(
  user_question = "Pourquoi ce dossier doit-il être analysé ?",
  client_data = client_example,
  prediction_result = prediction_result
)

checks <- data.frame(
  check = c(
    "model_loaded",
    "metadata_loaded",
    "logo_in_assets",
    "logo_in_www",
    "custom_css_exists",
    "prediction_probability_available",
    "risk_segment_available",
    "local_shap_available",
    "global_shap_file_exists",
    "agent_context_available",
    "fallback_answer_available"
  ),
  status = c(
    !is.null(best_model),
    !is.null(model_metadata),
    file.exists(file.path(project_root, "assets", "logo-cnam.png")),
    file.exists(file.path(project_root, "shiny_app", "www", "logo-cnam.png")),
    file.exists(file.path(project_root, "shiny_app", "www", "custom.css")),
    is.numeric(prediction_result$probability_yes),
    nzchar(prediction_result$risk_segment),
    !is.null(prediction_result$shap_top_features) && nrow(prediction_result$shap_top_features) > 0,
    file.exists(file.path(project_root, "reports", "shap_grouped_importance.csv")),
    nzchar(context_text),
    nzchar(fallback_answer)
  )
)

if (!all(checks$status)) {
  failed <- checks$check[!checks$status]
  stop("Tests composants échoués : ", paste(failed, collapse = ", "))
}

output <- c(
  "# Exemple de sortie agent",
  "",
  "## Contrôles techniques",
  "",
  paste(capture.output(print(checks)), collapse = "\n"),
  "",
  "## Dossier fictif",
  "",
  paste(capture.output(print(client_example)), collapse = "\n"),
  "",
  "## Résultat scoring",
  "",
  paste0("- Probabilité de sinistre : ", format_probability(prediction_result$probability_yes)),
  paste0("- Classe au seuil : ", prediction_result$predicted_class),
  paste0("- Seuil utilisé : ", format_probability(prediction_result$threshold_used)),
  paste0("- Segment : ", prediction_result$risk_segment, " (", prediction_result$segment_condition, ")"),
  paste0("- Taux de sinistre segment : ", format_probability(prediction_result$segment_claim_rate)),
  paste0("- Lift : ", round(prediction_result$segment_lift, 2)),
  paste0("- Recommandation : ", prediction_result$recommendation),
  "",
  "## Contributions SHAP locales",
  "",
  paste(capture.output(print(prediction_result$shap_top_features)), collapse = "\n"),
  "",
  "## Contexte transmis à l'agent",
  "",
  "```text",
  context_text,
  "```",
  "",
  "## Réponse agent",
  "",
  fallback_answer
)

writeLines(output, file.path(project_root, "reports", "example_agent_output.md"), useBytes = TRUE)

message("Test des composants Shiny/agent terminé.")
message("Fichier généré : reports/example_agent_output.md")
