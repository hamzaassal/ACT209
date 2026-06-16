# modules/mod_client_scoring.R
# Onglet unique de saisie, scoring et analyse metier.

mod_client_scoring_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      col_widths = c(5, 7),
      mod_input_client_ui(ns("client")),
      tagList(
        uiOutput(ns("empty_state")),
        mod_score_output_ui(ns("score"))
      )
    ),
    mod_risk_explanation_ui(ns("risk"))
  )
}

mod_client_scoring_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    client_data <- mod_input_client_server("client")

    prediction_result <- eventReactive(client_data()$calculate, {
      req(client_data()$data)
      predict_claim_risk(client_data()$data)
    }, ignoreInit = TRUE)

    output$empty_state <- renderUI({
      if (client_data()$calculate > 0) return(NULL)
      tags$div(
        class = "info-box",
        "Renseignez le dossier puis cliquez sur Calculer le score de risque pour afficher la probabilité de sinistre, le segment et la recommandation."
      )
    })

    mod_score_output_server("score", prediction_result)
    mod_risk_explanation_server("risk", client_data, prediction_result)

    list(
      client_data = client_data,
      prediction_result = prediction_result
    )
  })
}
