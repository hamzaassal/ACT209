# modules/mod_risk_analysis.R
# Restitution metier du score, de la priorisation et des graphiques de lift.

metric_tile <- function(label, value, detail = NULL, class = "") {
  div(
    class = paste("metric-tile", class),
    span(class = "metric-label", label),
    strong(value),
    if (!is.null(detail)) tags$small(detail)
  )
}

safe_img <- function(src, alt) {
  div(
    class = "figure-card",
    tags$img(src = src, alt = alt),
    tags$span(alt)
  )
}

mod_risk_analysis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("analysis_panel")),
    card(
      class = "content-card methodology-card",
      card_header("Pourquoi raisonner en score ?"),
      p("Dans un contexte de sinistres rares, la classification binaire directe est limitee. L'approche retenue consiste a utiliser le modele comme un score de risque permettant d'identifier les dossiers les plus exposes."),
      div(
        class = "method-grid",
        div(strong("Score"), span("Probabilite estimee de sinistre.")),
        div(strong("Segment"), span("Position du dossier parmi les scores les plus eleves.")),
        div(strong("Lift"), span("Sur-sinistralite du segment par rapport au taux moyen."))
      )
    ),
    uiOutput(ns("lift_panel"))
  )
}

mod_risk_analysis_server <- function(id, prediction_result, prediction_error = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    output$analysis_panel <- renderUI({
      if (!is.null(prediction_error())) {
        return(div(class = "alert-box", strong("Analyse indisponible : "), prediction_error()))
      }

      result <- prediction_result()
      if (is.null(result)) {
        return(empty_state(
          "Aucun dossier score",
          "Calculez d'abord un score dans l'onglet Scoring d'un dossier pour afficher l'analyse du risque."
        ))
      }

      attention <- dplyr::case_when(
        result$risk_segment_key %in% c("very_high", "high") ~ "Analyse complementaire prioritaire",
        result$risk_segment_key %in% c("moderate_high", "moderate") ~ "Revue metier recommandee",
        TRUE ~ "Traitement standard selon les regles internes"
      )

      tagList(
        div(
          class = "kpi-grid",
          metric_tile("Score de risque estime", format_probability(result$probability_yes), "Probabilite de sinistre", result$risk_css_class),
          metric_tile("Niveau de risque", result$risk_level, result$segment_condition, result$risk_css_class),
          metric_tile("Segment de risque", result$segment_condition, "Ranking du portefeuille", result$risk_css_class),
          metric_tile("Lift", paste0("x", round(result$segment_lift, 2)), "Par rapport au taux moyen", result$risk_css_class)
        ),
        card(
          class = "content-card interpretation-card",
          card_header("Interpretation metier"),
          p("Ce dossier appartient a un segment dont le taux de sinistre observe est compare au taux moyen du portefeuille. Le score doit etre interprete comme un signal de priorisation et non comme une certitude de sinistre."),
          div(
            class = "analysis-grid",
            div(strong("Taux moyen portefeuille"), span(format_probability(result$baseline_claim_rate))),
            div(strong("Taux du segment"), span(format_probability(result$segment_claim_rate))),
            div(strong("Niveau d'attention"), span(attention)),
            div(strong("Seuil operationnel"), span(format_probability(result$threshold_used)))
          ),
          div(
            class = paste("recommendation-box", result$risk_css_class),
            strong("Recommandation underwriting : "),
            result$recommendation
          )
        )
      )
    })

    output$lift_panel <- renderUI({
      lift_available <- !is.null(lift_table) && nrow(lift_table) > 0
      lift_curve <- file.exists(file.path(PROJECT_ROOT, "reports", "figures", "lift_curve.png"))
      gains_curve <- file.exists(file.path(PROJECT_ROOT, "reports", "figures", "cumulative_gains_chart.png"))
      score_dist <- file.exists(file.path(PROJECT_ROOT, "reports", "figures", "score_distribution_by_claim_status.png"))

      card(
        class = "content-card",
        card_header("Analyse ranking / lift"),
        if (lift_available) {
          tagList(
            p("Les segments ci-dessous montrent la concentration historique des sinistres dans les dossiers ayant les scores les plus eleves."),
            tableOutput(session$ns("lift_table"))
          )
        } else {
          div(class = "info-box", "Le tableau de lift n'est pas disponible. L'application conserve l'explication metier sans graphique.")
        },
        div(
          class = "figure-grid",
          if (lift_curve) safe_img("report_figures/lift_curve.png", "Courbe de lift"),
          if (gains_curve) safe_img("report_figures/cumulative_gains_chart.png", "Cumulative gains chart"),
          if (score_dist) safe_img("report_figures/score_distribution_by_claim_status.png", "Distribution des scores")
        )
      )
    })

    output$lift_table <- renderTable({
      req(!is.null(lift_table), nrow(lift_table) > 0)
      display <- lift_table
      names(display) <- gsub("_", " ", names(display))
      head(display, 5)
    }, striped = TRUE, bordered = FALSE, spacing = "s")
  })
}
