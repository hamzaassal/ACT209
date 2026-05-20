# 08_test_shiny_agent_components.R
# Test léger des composants Shiny et agent IA sans lancer l'interface.

required_packages <- c("shiny", "bslib", "yaml", "dplyr", "tidymodels")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Packages requis manquants : ", paste(missing_packages, collapse = ", "),
    "\nInstallez-les avec install.packages() puis relancez le test."
  )
}

project_root <- normalizePath(".", winslash = "/", mustWork = TRUE)
source(file.path(project_root, "shiny_app", "global.R"), local = FALSE)

dir.create(file.path(project_root, "reports"), showWarnings = FALSE, recursive = TRUE)

client_example <- data.frame(
  agency = "C2B",
  agency_type = "Airlines",
  distribution_channel = "Online",
  product_name = "Cancellation Plan",
  duration = 15,
  destination = "SINGAPORE",
  net_sales = 80,
  commision_in_value = 20,
  gender = "F",
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

output <- c(
  "# Exemple de sortie agent",
  "",
  "## Dossier fictif",
  "",
  paste(capture.output(print(client_example)), collapse = "\n"),
  "",
  "## Résultat scoring",
  "",
  paste0("- Probabilité de sinistre : ", format_pct(prediction_result$probability_yes, 2)),
  paste0("- Classe au seuil : ", prediction_result$predicted_class),
  paste0("- Seuil utilisé : ", format_pct(prediction_result$threshold_used, 2)),
  paste0("- Segment : ", prediction_result$risk_segment, " (", prediction_result$segment_condition, ")"),
  paste0("- Taux de sinistre segment : ", format_pct(prediction_result$segment_claim_rate, 2)),
  paste0("- Lift : ", round(prediction_result$segment_lift, 2)),
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
