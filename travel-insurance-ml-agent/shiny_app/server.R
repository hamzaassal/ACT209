# server.R
# Serveur principal de l'application.

app_server <- function(input, output, session) {
  mod_stat_exploration_server("stats")
  mod_ml_summary_server("ml")

  scoring <- mod_client_scoring_server("scoring")

  mod_agent_chat_server("agent", scoring$client_data, scoring$prediction_result)
}
