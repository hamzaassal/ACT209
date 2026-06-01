# modules/mod_score_output.R
# Module d'affichage du resultat de scoring.

empty_state <- function(title, text) {
  div(
    class = "empty-state",
    h3(title),
    p(text)
  )
}

score_metric_card <- function(label, value, detail = NULL, class = "") {
  div(
    class = paste("score-card", class),
    span(class = "score-label", label),
    strong(value),
    if (!is.null(detail)) tags$small(detail)
  )
}

mod_score_output_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("error")),
    uiOutput(ns("score_panel"))
  )
}

mod_score_output_server <- function(id, prediction_result, prediction_error = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    output$error <- renderUI({
      if (is.null(prediction_error())) return(NULL)
      div(class = "alert-box", strong("Erreur de scoring : "), prediction_error())
    })

    output$score_panel <- renderUI({
      result <- prediction_result()
      if (is.null(result)) {
        return(empty_state(
          "Aucun score calcule",
          "Saisissez un dossier puis cliquez sur Calculer le score de risque pour afficher les resultats."
        ))
      }

      tagList(
        layout_columns(
          col_widths = c(6, 6),
          score_metric_card("Probabilite estimee", format_probability(result$probability_yes), "Risque de sinistre", result$risk_css_class),
          score_metric_card("Segment", result$risk_segment, paste(result$segment_condition, "- lift x", round(result$segment_lift, 2)), result$risk_css_class)
        ),
        card(
          class = "content-card",
          card_header("Lecture essentielle"),
          div(
            class = paste("recommendation-box", result$risk_css_class),
            tags$p(strong("Classe au seuil operationnel : "), toupper(result$predicted_class), " avec seuil ", format_probability(result$threshold_used)),
            tags$p(strong("Taux historique du segment : "), format_probability(result$segment_claim_rate), " contre ", format_probability(result$baseline_claim_rate), " en moyenne."),
            tags$p(strong("Recommandation automatique : "), result$recommendation),
            tags$p(strong("Modele utilise : "), result$model_label),
            tags$p(strong("Choix applicatif : "), "modele sans variable tarifaire afin d'eviter une recommandation fondee sur une prime deja calculee.")
          ),
          div(
            class = "warning-box",
            strong("Prudence : "),
            "ce score aide a prioriser l'analyse. Il ne constitue pas une decision contractuelle automatique."
          )
        )
      )
    })
  })
}
