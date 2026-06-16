# modules/mod_ml_summary.R
# Synthese Machine Learning basee sur les artefacts existants.

mod_ml_summary_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Champion", textOutput(ns("champion")), showcase = "ML"),
      value_box("ROC AUC", textOutput(ns("roc_auc")), showcase = "AUC"),
      value_box("Recall", textOutput(ns("recall")), showcase = "R"),
      value_box("F1-score", textOutput(ns("f1")), showcase = "F1")
    ),
    navset_card_tab(
      nav_panel(
        "Problématique",
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Objectif ML"),
            p("Le modèle prédit la survenance d'un sinistre d'assurance voyage, c'est-à-dire la modalité positive claim_status = yes."),
            p("Dans ce contexte actuariel, la sortie doit être lue comme un score de priorisation du risque et non comme une décision automatique.")
          ),
          card(
            card_header("Déséquilibre des classes"),
            DT::DTOutput(ns("class_balance"))
          )
        )
      ),
      nav_panel(
        "Comparaison modèles",
        card(
          card_header("Métriques de validation croisée"),
          DT::DTOutput(ns("model_comparison"))
        ),
        card(
          card_header("Classement des modèles"),
          DT::DTOutput(ns("model_ranking"))
        )
      ),
      nav_panel(
        "Champion",
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Métriques finales sur test set"),
            DT::DTOutput(ns("final_metrics"))
          ),
          card(
            card_header("Matrice de confusion"),
            plotly::plotlyOutput(ns("confusion_plot"), height = "320px")
          )
        ),
        card(
          card_header("Interprétation métier"),
          uiOutput(ns("business_interpretation"))
        )
      )
    )
  )
}

mod_ml_summary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    final_metrics_reactive <- reactive({
      validate(need(!is.null(final_metrics), "Métriques finales indisponibles : relancez scripts/05_model_evaluation.R."))
      final_metrics
    })

    output$champion <- renderText({
      if (!is.null(model_metadata) && !is.null(model_metadata$champion$config_id)) {
        return(model_metadata$champion$config_id)
      }
      if (!is.null(model_ranking) && nrow(model_ranking) > 0) {
        return(model_ranking$config_id[[1]])
      }
      "Indisponible"
    })

    metric_value <- function(metric_name) {
      metrics <- final_metrics_reactive()
      value <- metrics %>% filter(.metric == metric_name) %>% pull(.estimate)
      if (length(value) == 0 || is.na(value[[1]])) return("NA")
      round(value[[1]], 4)
    }

    output$roc_auc <- renderText(metric_value("roc_auc"))
    output$recall <- renderText(metric_value("recall"))
    output$f1 <- renderText(metric_value("f_meas"))

    output$class_balance <- DT::renderDT({
      validate(need(!is.null(class_balance_summary), "Résumé de déséquilibre indisponible."))
      DT::datatable(round_numeric(class_balance_summary), rownames = FALSE, options = list(dom = "tip", pageLength = 6))
    })

    output$model_comparison <- DT::renderDT({
      validate(need(!is.null(model_comparison), "Comparaison modèles indisponible : relancez scripts/04_model_training.R."))
      display <- model_comparison %>%
        select(config_id, model, strategy, status, .metric, mean, reason) %>%
        arrange(config_id, .metric)
      DT::datatable(round_numeric(display), rownames = FALSE, filter = "top", options = list(pageLength = 12, scrollX = TRUE))
    })

    output$model_ranking <- DT::renderDT({
      validate(need(!is.null(model_ranking), "Classement modèles indisponible : relancez scripts/04_model_training.R."))
      DT::datatable(round_numeric(model_ranking), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })

    output$final_metrics <- DT::renderDT({
      display <- final_metrics_reactive() %>%
        select(config_id, model, strategy, .metric, .estimate)
      DT::datatable(round_numeric(display), rownames = FALSE, options = list(dom = "tip", pageLength = 8))
    })

    output$confusion_plot <- plotly::renderPlotly({
      validate(need(!is.null(confusion_matrix), "Matrice de confusion indisponible : relancez scripts/05_model_evaluation.R."))
      plotly::plot_ly(
        confusion_matrix,
        x = ~Truth,
        y = ~Prediction,
        z = ~n,
        type = "heatmap",
        colors = "Blues",
        text = ~n,
        texttemplate = "%{text}"
      ) %>%
        plotly::layout(xaxis = list(title = "Réel"), yaxis = list(title = "Prédit"))
    })

    output$business_interpretation <- renderUI({
      metrics <- final_metrics_reactive()
      roc <- metrics %>% filter(.metric == "roc_auc") %>% pull(.estimate)
      pr <- metrics %>% filter(.metric == "pr_auc") %>% pull(.estimate)
      rec <- metrics %>% filter(.metric == "recall") %>% pull(.estimate)
      prec <- metrics %>% filter(.metric == "precision") %>% pull(.estimate)
      f1 <- metrics %>% filter(.metric == "f_meas") %>% pull(.estimate)
      threshold <- if (!is.null(model_metadata) && !is.null(model_metadata$decision_threshold$threshold)) {
        model_metadata$decision_threshold$threshold
      } else {
        NA_real_
      }

      tagList(
        p("La problématique est fortement déséquilibrée : les sinistres représentent une faible part du portefeuille. Les métriques de ranking comme la ROC AUC et la PR AUC sont donc plus informatives que l'accuracy seule."),
        tags$ul(
          tags$li(strong("ROC AUC : "), round(roc, 4), " pour mesurer la capacité de classement global."),
          tags$li(strong("PR AUC : "), round(pr, 4), " utile lorsque la classe positive est rare."),
          tags$li(strong("Recall : "), round(rec, 4), " pour mesurer la part de sinistres détectés au seuil retenu."),
          tags$li(strong("Precision : "), round(prec, 4), " pour mesurer la qualité des alertes positives."),
          tags$li(strong("F1-score : "), round(f1, 4), " comme compromis precision/recall."),
          tags$li(strong("Seuil calibré sauvegardé : "), ifelse(is.na(threshold), "indisponible", format_pct(threshold, 2)))
        ),
        p("D'un point de vue métier, le modèle doit prioriser les dossiers à analyser plutôt que produire une décision automatique. Les alertes doivent rester confrontées aux règles internes, exclusions contractuelles et informations complémentaires.")
      )
    })
  })
}
