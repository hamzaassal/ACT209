# modules/mod_score_output.R
# Module d'affichage du resultat de scoring.

mod_score_output_ui <- function(id) {
  ns <- NS(id)
  tagList(
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Score de risque", textOutput(ns("score")), showcase = "P"),
      value_box("Segment", textOutput(ns("segment")), showcase = "!"),
      value_box("Lift", textOutput(ns("lift")), showcase = "x"),
      value_box("Taux segment", textOutput(ns("claim_rate")), showcase = "%")
    ),
    card(
      card_header("Recommandation underwriting synthetique"),
      uiOutput(ns("recommendation")),
      tags$div(class = "warning-box", "Ce score est une aide a la decision. Le modele ne remplace pas l'analyse de l'underwriter.")
    )
  )
}

mod_score_output_server <- function(id, prediction_result) {
  moduleServer(id, function(input, output, session) {
    output$score <- renderText({
      result <- prediction_result()
      if (is.null(result)) return("En attente")
      format_pct(result$probability_yes, 2)
    })

    output$segment <- renderText({
      result <- prediction_result()
      if (is.null(result)) return("En attente")
      result$risk_segment
    })

    output$lift <- renderText({
      result <- prediction_result()
      if (is.null(result)) return("En attente")
      paste0("x", round(result$segment_lift, 2))
    })

    output$claim_rate <- renderText({
      result <- prediction_result()
      if (is.null(result)) return("En attente")
      format_pct(result$segment_claim_rate, 2)
    })

    output$recommendation <- renderUI({
      result <- prediction_result()
      if (is.null(result)) {
        return(tags$div(class = "info-box", "Aucun score calcule pour le moment."))
      }
      tags$div(
        class = "recommendation-box",
        tags$p(strong("Classe predite au seuil : "), result$predicted_class),
        tags$p(strong("Seuil operationnel utilise : "), format_pct(result$threshold_used, 2)),
        tags$p(strong("Segment de ranking : "), result$segment_condition),
        tags$p(strong("Recommandation : "), result$recommendation_level)
      )
    })
  })
}
