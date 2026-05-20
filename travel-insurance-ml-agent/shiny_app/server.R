# server.R
# Serveur principal de l'application.

app_server <- function(input, output, session) {
  client_data <- mod_input_client_server("client")

  prediction_result <- eventReactive(client_data()$calculate, {
    req(client_data()$data)
    predict_claim_risk(client_data()$data)
  }, ignoreInit = TRUE)

  mod_score_output_server("score", prediction_result)
  mod_risk_explanation_server("risk", client_data, prediction_result)
  mod_agent_chat_server("agent", client_data, prediction_result)
}
