# modules/mod_pricing_simulation.R
# Simulation tarifaire simple a partir du score client.

mod_pricing_simulation_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("score_required_message")),
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Hypotheses tarifaires"),
        numericInput(ns("avg_claim_cost"), "Cout moyen estime d'un sinistre", value = 1000, min = 0, step = 50),
        numericInput(ns("management_fee"), "Frais de gestion (%)", value = 15, min = 0, max = 95, step = 1),
        numericInput(ns("safety_margin"), "Marge de securite (%)", value = 10, min = 0, max = 95, step = 1),
        numericInput(ns("commission"), "Commission (%)", value = 10, min = 0, max = 95, step = 1),
        numericInput(ns("tax"), "Taxe (%)", value = 9, min = 0, max = 100, step = 1)
      ),
      tagList(
        layout_columns(
          col_widths = c(3, 3, 3, 3),
          value_box("Probabilite", textOutput(ns("probability")), showcase = "P"),
          value_box("Prime pure", textOutput(ns("pure_premium")), showcase = "PP"),
          value_box("Prime HT", textOutput(ns("loaded_premium")), showcase = "HT"),
          value_box("Prime TTC", textOutput(ns("commercial_premium")), showcase = "TTC")
        ),
        card(
          card_header("Comparaison avec la prime observee"),
          DT::DTOutput(ns("pricing_table"))
        ),
        card(
          card_header("Interpretation et recommandation"),
          uiOutput(ns("pricing_interpretation"))
        )
      )
    )
  )
}

mod_pricing_simulation_server <- function(id, client_data, prediction_result) {
  moduleServer(id, function(input, output, session) {
    has_score <- reactive({
      !is.null(prediction_result()) && !is.null(client_data()$data)
    })

    output$score_required_message <- renderUI({
      if (isTRUE(has_score())) return(NULL)
      tags$div(
        class = "info-box",
        "Calculez d'abord un score dans l'onglet Scoring client pour activer la simulation tarifaire."
      )
    })

    pricing <- reactive({
      validate(need(has_score(), "Score client indisponible. Calculez d'abord un score dans l'onglet Scoring client."))

      result <- prediction_result()
      client <- client_data()$data

      avg_claim_cost <- as.numeric(input$avg_claim_cost)
      management_fee <- as.numeric(input$management_fee) / 100
      safety_margin <- as.numeric(input$safety_margin) / 100
      commission <- as.numeric(input$commission) / 100
      tax <- as.numeric(input$tax) / 100

      validate(need(!is.na(avg_claim_cost) && avg_claim_cost >= 0, "Le cout moyen de sinistre doit etre positif ou nul."))
      validate(need(commission < 1, "La commission doit etre inferieure a 100 %."))

      probability <- as.numeric(result$probability_yes)
      pure_premium <- probability * avg_claim_cost
      loaded_before_commission <- pure_premium * (1 + management_fee + safety_margin)
      loaded_premium <- loaded_before_commission / (1 - commission)
      commercial_premium <- loaded_premium * (1 + tax)

      observed_premium <- if ("net_sales" %in% names(client)) as.numeric(client$net_sales[[1]]) else NA_real_
      gap <- if (!is.na(observed_premium)) observed_premium - commercial_premium else NA_real_
      observed_ratio <- if (!is.na(observed_premium) && commercial_premium > 0) observed_premium / commercial_premium else NA_real_

      interpretation <- dplyr::case_when(
        is.na(observed_ratio) ~ "Comparaison indisponible",
        observed_ratio < 0.85 ~ "Sous-tarife",
        observed_ratio <= 1.20 ~ "Coherent",
        TRUE ~ "Prudent / sur-tarife"
      )

      recommendation <- dplyr::case_when(
        result$risk_segment_key == "very_high" ~ "Demander une analyse complementaire approfondie ; envisager une surprime, un refus ou l'exclusion de certaines garanties.",
        interpretation == "Sous-tarife" & probability >= 0.05 ~ "Appliquer une surprime ou demander une analyse complementaire.",
        interpretation == "Sous-tarife" ~ "Verifier le tarif standard et envisager un ajustement.",
        interpretation == "Coherent" & probability < 0.05 ~ "Accepter au tarif standard sous reserve des regles internes.",
        interpretation == "Coherent" ~ "Accepter avec revue underwriting standard.",
        TRUE ~ "Tarif prudent ; verifier la competitivite commerciale avant decision."
      )

      list(
        probability = probability,
        pure_premium = pure_premium,
        loaded_premium = loaded_premium,
        commercial_premium = commercial_premium,
        observed_premium = observed_premium,
        gap = gap,
        observed_ratio = observed_ratio,
        interpretation = interpretation,
        recommendation = recommendation,
        risk_segment = result$risk_segment,
        threshold_used = result$threshold_used
      )
    })

    currency <- function(x) {
      if (is.na(x)) return("NA")
      paste0(round(x, 2), " EUR")
    }

    output$probability <- renderText({
      format_pct(pricing()$probability, 2)
    })

    output$pure_premium <- renderText({
      currency(pricing()$pure_premium)
    })

    output$loaded_premium <- renderText({
      currency(pricing()$loaded_premium)
    })

    output$commercial_premium <- renderText({
      currency(pricing()$commercial_premium)
    })

    output$pricing_table <- DT::renderDT({
      p <- pricing()
      table <- data.frame(
        indicateur = c(
          "Probabilite de sinistre",
          "Prime pure",
          "Prime chargee avant taxe",
          "Prime commerciale TTC",
          "Prime observee net_sales",
          "Ecart observe - technique",
          "Ratio observe / technique"
        ),
        valeur = c(
          format_pct(p$probability, 2),
          currency(p$pure_premium),
          currency(p$loaded_premium),
          currency(p$commercial_premium),
          currency(p$observed_premium),
          currency(p$gap),
          ifelse(is.na(p$observed_ratio), "NA", round(p$observed_ratio, 3))
        )
      )
      DT::datatable(table, rownames = FALSE, options = list(dom = "t", paging = FALSE))
    })

    output$pricing_interpretation <- renderUI({
      p <- pricing()
      tagList(
        tags$p(strong("Segment de risque : "), p$risk_segment),
        tags$p(strong("Lecture tarifaire : "), p$interpretation),
        tags$p(strong("Recommandation : "), p$recommendation),
        tags$div(
          class = "warning-box",
          "Cette simulation est un indicateur actuariel simplifie. Elle ne remplace pas une etude tarifaire complete ni les regles de souscription."
        )
      )
    })
  })
}
