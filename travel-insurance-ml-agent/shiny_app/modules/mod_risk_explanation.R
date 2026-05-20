# modules/mod_risk_explanation.R
# Module d'analyse métier déterministe.

mod_risk_explanation_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header("Interprétation métier du score"),
    uiOutput(ns("alert")),
    verbatimTextOutput(ns("explanation")),
    tags$hr(),
    tags$p(strong("Message de prudence")),
    tags$p("Les résultats doivent être interprétés au regard des règles internes de souscription et des informations contractuelles disponibles.")
  )
}

mod_risk_explanation_server <- function(id, client_data, prediction_result) {
  moduleServer(id, function(input, output, session) {
    output$alert <- renderUI({
      req(prediction_result())
      condition <- prediction_result()$segment_condition
      if (condition %in% c("Top 1 %", "Top 5 %", "Top 10 %")) {
        tags$div(class = "alert-box", paste("Alerte :", condition, "des scores les plus élevés. Revue underwriting recommandée."))
      } else {
        tags$div(class = "info-box", "Aucun signal de priorisation extrême selon le segment de score.")
      }
    })

    output$explanation <- renderText({
      req(client_data()$data, prediction_result())
      explain_prediction(prediction_result(), client_data()$data)
    })
  })
}
