# modules/mod_score_output.R
# Module d'affichage du résultat de scoring.

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
      card_header("Recommandation underwriting synthétique"),
      uiOutput(ns("recommendation")),
      tags$div(class = "warning-box", "Ce score est une aide à la décision. Le modèle ne remplace pas l'analyse de l'underwriter.")
    )
  )
}

mod_score_output_server <- function(id, prediction_result) {
  moduleServer(id, function(input, output, session) {
    output$score <- renderText({
      req(prediction_result())
      format_pct(prediction_result()$probability_yes, 2)
    })

    output$segment <- renderText({
      req(prediction_result())
      prediction_result()$risk_segment
    })

    output$lift <- renderText({
      req(prediction_result())
      paste0("x", round(prediction_result()$segment_lift, 2))
    })

    output$claim_rate <- renderText({
      req(prediction_result())
      format_pct(prediction_result()$segment_claim_rate, 2)
    })

    output$recommendation <- renderUI({
      req(prediction_result())
      tags$div(
        class = "recommendation-box",
        tags$p(strong("Classe prédite au seuil : "), prediction_result()$predicted_class),
        tags$p(strong("Seuil opérationnel utilisé : "), format_pct(prediction_result()$threshold_used, 2)),
        tags$p(strong("Segment de ranking : "), prediction_result()$segment_condition),
        tags$p(strong("Recommandation : "), prediction_result()$recommendation_level)
      )
    })
  })
}
