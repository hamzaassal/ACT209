# server.R
# Serveur principal de l'application.

app_server <- function(input, output, session) {
  client_data <- mod_input_client_server("client")
  prediction_store <- reactiveVal(NULL)
  error_store <- reactiveVal(NULL)

  run_scoring <- function(show_success = FALSE) {
    req(client_data()$data)
    error_store(NULL)

    result <- tryCatch(
      predict_claim_risk(client_data()$data),
      error = function(e) {
        error_store(conditionMessage(e))
        showNotification(
          paste("Scoring impossible :", conditionMessage(e)),
          type = "error",
          duration = 8
        )
        NULL
      }
    )

    if (!is.null(result)) {
      prediction_store(result)
      if (isTRUE(show_success)) {
        showNotification("Score de risque calculé avec succès.", type = "message", duration = 4)
      }
    }
  }

  observeEvent(client_data()$calculate, {
    run_scoring(show_success = TRUE)
  }, ignoreInit = TRUE)

  # Une fois le premier score calculé, les modifications du formulaire
  # actualisent immédiatement le score et les recommandations affichées.
  observeEvent(client_data()$data, {
    if (is.null(prediction_store())) return(NULL)
    run_scoring(show_success = FALSE)
  }, ignoreInit = TRUE)

  prediction_result <- reactive(prediction_store())
  prediction_error <- reactive(error_store())

  mod_score_output_server("score", prediction_result, prediction_error)
  mod_risk_analysis_server("risk_analysis", prediction_result, prediction_error)
  mod_agent_chat_server("agent", client_data, prediction_result, prediction_error)
}
