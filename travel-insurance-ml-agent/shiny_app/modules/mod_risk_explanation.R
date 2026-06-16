# modules/mod_risk_explanation.R
# Module d'analyse metier, priorisation et SHAP.

mod_risk_explanation_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("priority_panel")),
    card(
      class = "content-card",
      card_header("Contributions SHAP du dossier"),
      uiOutput(ns("local_shap"))
    ),
    card(
      class = "content-card",
      card_header("Lecture portefeuille : scoring et lift"),
      uiOutput(ns("lift_table"))
    )
  )
}

mod_risk_explanation_server <- function(id, client_data, prediction_result, prediction_error = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    output$priority_panel <- renderUI({
      if (!is.null(prediction_error())) {
        return(div(class = "alert-box", strong("Analyse indisponible : "), prediction_error()))
      }

      result <- prediction_result()
      if (is.null(result)) {
        return(empty_state(
          "Analyse en attente",
          "Calculez un score dans l'onglet Saisie dossier pour obtenir l'interpretation metier."
        ))
      }

      is_priority <- result$segment_condition %in% c("Top 1 %", "Top 5 %", "Top 10 %")
      alert_class <- if (is_priority) "alert-box" else "info-box"
      alert_text <- if (is_priority) {
        paste("Signal prioritaire :", result$segment_condition, "des scores les plus eleves. Une revue underwriting est recommandee.")
      } else {
        paste("Segment", result$segment_condition, ": le dossier reste a interpreter selon les regles internes.")
      }

      card(
        class = "content-card",
        card_header("Interpretation du dossier"),
        div(class = alert_class, alert_text),
        div(
          class = "analysis-grid",
          div(strong("Score"), span(format_probability(result$probability_yes))),
          div(strong("Segment"), span(paste(result$risk_segment, "-", result$segment_condition))),
          div(strong("Lift"), span(paste0("x", round(result$segment_lift, 2)))),
          div(strong("Taux segment"), span(format_probability(result$segment_claim_rate)))
        ),
        p(explain_prediction(result, client_data()$data)),
        p(class = "technical-note", result$score_threshold_source)
      )
    })

    output$local_shap <- renderUI({
      result <- prediction_result()
      if (is.null(result)) {
        return(p("Les contributions SHAP seront affichees apres calcul du score."))
      }
      shap <- result$shap_top_features
      if (is.null(shap) || nrow(shap) == 0) {
        return(p("Contributions SHAP locales indisponibles pour ce dossier."))
      }

      tagList(
        p("Les contributions SHAP indiquent les groupes de variables qui pesent le plus dans le score du dossier. Une contribution positive augmente le score de risque, une contribution negative le diminue."),
        tableOutput(session$ns("local_shap_table"))
      )
    })

    output$local_shap_table <- renderTable({
      req(prediction_result())
      shap <- prediction_result()$shap_top_features
      req(shap)
      shap %>%
        dplyr::transmute(
          Variable = feature_group,
          Effet = direction,
          Contribution = round(shap_value, 4)
        )
    }, striped = TRUE, bordered = FALSE, digits = 4)

    output$lift_table <- renderUI({
      if (is.null(lift_table)) {
        return(p("Le tableau de lift n'est pas disponible dans reports/risk_ranking_lift_table.csv."))
      }

      tagList(
        p("Les segments de score les plus eleves concentrent une part importante des sinistres observes. C'est la justification metier de l'approche scoring/ranking."),
        tableOutput(session$ns("lift_table_rendered"))
      )
    })

    output$lift_table_rendered <- renderTable({
      req(lift_table)
      lift_table
    }, striped = TRUE, bordered = FALSE, digits = 4)
  })
}
