# modules/mod_risk_explanation.R
# Module d'analyse metier deterministe.

mod_risk_explanation_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header("Interpretation metier du score"),
    uiOutput(ns("alert")),
    verbatimTextOutput(ns("explanation")),
    tags$hr(),
    tags$p(strong("Message de prudence")),
    tags$p("Les resultats doivent etre interpretes au regard des regles internes de souscription et des informations contractuelles disponibles.")
  )
}

mod_risk_explanation_server <- function(id, client_data, prediction_result) {
  moduleServer(id, function(input, output, session) {
    output$alert <- renderUI({
      result <- prediction_result()
      if (is.null(result)) {
        return(tags$div(class = "info-box", "Calculez un score pour afficher l'analyse metier du dossier."))
      }

      condition <- result$segment_condition
      if (condition %in% c("Top 1 %", "Top 5 %", "Top 10 %")) {
        tags$div(class = "alert-box", paste("Alerte :", condition, "des scores les plus eleves. Revue underwriting recommandee."))
      } else {
        tags$div(class = "info-box", "Aucun signal de priorisation extreme selon le segment de score.")
      }
    })

    output$explanation <- renderText({
      result <- prediction_result()
      if (is.null(result)) {
        return("En attente du calcul de score.")
      }
      req(client_data()$data)
      explain_prediction(result, client_data()$data)
    })
  })
}
