# modules/mod_stat_exploration.R
# Exploration statistique des donnees Travel Insurance.

mod_stat_exploration_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box("Observations", textOutput(ns("n_rows")), showcase = "n"),
      value_box("Variables", textOutput(ns("n_cols")), showcase = "#"),
      value_box("Taux de sinistre", textOutput(ns("claim_rate")), showcase = "%")
    ),
    card(
      card_header("Filtres"),
      layout_columns(
        col_widths = c(3, 3, 3, 3),
        selectInput(ns("product_name"), "Produit", choices = "Tous"),
        selectInput(ns("agency"), "Agence", choices = "Toutes"),
        selectInput(ns("distribution_channel"), "Canal", choices = "Tous"),
        selectInput(ns("destination"), "Destination", choices = "Toutes")
      )
    ),
    navset_card_tab(
      nav_panel(
        "Numérique",
        layout_columns(
          col_widths = c(4, 8),
          card(
            card_header("Variable"),
            selectInput(ns("numeric_var"), "Variable numérique", choices = character(0))
          ),
          card(
            card_header("Distribution"),
            plotly::plotlyOutput(ns("numeric_distribution"), height = "360px")
          )
        ),
        card(
          card_header("Statistiques univariées numériques"),
          DT::DTOutput(ns("numeric_summary"))
        )
      ),
      nav_panel(
        "Catégoriel",
        layout_columns(
          col_widths = c(4, 8),
          card(
            card_header("Variable"),
            selectInput(ns("categorical_var"), "Variable catégorielle", choices = character(0))
          ),
          card(
            card_header("Répartition"),
            plotly::plotlyOutput(ns("categorical_distribution"), height = "360px")
          )
        ),
        card(
          card_header("Statistiques univariées catégorielles"),
          DT::DTOutput(ns("categorical_summary"))
        )
      ),
      nav_panel(
        "Cible",
        layout_columns(
          col_widths = c(5, 7),
          card(
            card_header("Claim Status"),
            plotly::plotlyOutput(ns("target_distribution"), height = "320px")
          ),
          card(
            card_header("Taux de sinistre par variable"),
            selectInput(
              ns("rate_var"),
              "Variable d'analyse",
              choices = c("product_name", "agency", "destination", "distribution_channel", "age", "duration", "net_sales")
            ),
            plotly::plotlyOutput(ns("claim_rate_plot"), height = "320px")
          )
        ),
        card(
          card_header("Tableau des taux de sinistre"),
          DT::DTOutput(ns("claim_rate_table"))
        )
      )
    )
  )
}

mod_stat_exploration_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    has_data <- reactive(!is.null(data_clean) && "claim_status" %in% names(data_clean))

    shiny::observe({
      req(has_data())
      updateSelectInput(session, "product_name", choices = c("Tous", sort(unique(data_clean$product_name))))
      updateSelectInput(session, "agency", choices = c("Toutes", sort(unique(data_clean$agency))))
      updateSelectInput(session, "distribution_channel", choices = c("Tous", sort(unique(data_clean$distribution_channel))))
      updateSelectInput(session, "destination", choices = c("Toutes", sort(unique(data_clean$destination))))

      numeric_vars <- names(data_clean)[vapply(data_clean, is.numeric, logical(1))]
      categorical_vars <- names(data_clean)[vapply(data_clean, function(x) is.character(x) || is.factor(x), logical(1))]
      categorical_vars <- setdiff(categorical_vars, "claim_status")

      updateSelectInput(session, "numeric_var", choices = numeric_vars, selected = numeric_vars[[1]])
      updateSelectInput(session, "categorical_var", choices = categorical_vars, selected = categorical_vars[[1]])
    })

    filtered_data <- reactive({
      validate(need(has_data(), "Données nettoyées indisponibles : relancez scripts/01_import_data.R."))

      df <- data_clean
      if (!is.null(input$product_name) && input$product_name != "Tous") {
        df <- df %>% filter(product_name == input$product_name)
      }
      if (!is.null(input$agency) && input$agency != "Toutes") {
        df <- df %>% filter(agency == input$agency)
      }
      if (!is.null(input$distribution_channel) && input$distribution_channel != "Tous") {
        df <- df %>% filter(distribution_channel == input$distribution_channel)
      }
      if (!is.null(input$destination) && input$destination != "Toutes") {
        df <- df %>% filter(destination == input$destination)
      }
      df
    })

    output$n_rows <- renderText({
      format(nrow(filtered_data()), big.mark = " ")
    })

    output$n_cols <- renderText({
      ncol(filtered_data())
    })

    output$claim_rate <- renderText({
      format_pct(mean(filtered_data()$claim_status == "yes"), 2)
    })

    output$numeric_summary <- DT::renderDT({
      df <- filtered_data()
      numeric_vars <- names(df)[vapply(df, is.numeric, logical(1))]
      validate(need(length(numeric_vars) > 0, "Aucune variable numérique disponible."))

      summary <- purrr::map_dfr(numeric_vars, function(variable) {
        x <- df[[variable]]
        tibble::tibble(
          variable = variable,
          min = min(x, na.rm = TRUE),
          q1 = as.numeric(stats::quantile(x, 0.25, na.rm = TRUE)),
          median = stats::median(x, na.rm = TRUE),
          mean = mean(x, na.rm = TRUE),
          q3 = as.numeric(stats::quantile(x, 0.75, na.rm = TRUE)),
          max = max(x, na.rm = TRUE),
          sd = stats::sd(x, na.rm = TRUE),
          missing = sum(is.na(x))
        )
      })

      DT::datatable(round_numeric(summary), rownames = FALSE, options = list(pageLength = 8, scrollX = TRUE))
    })

    output$numeric_distribution <- plotly::renderPlotly({
      df <- filtered_data()
      req(input$numeric_var)
      validate(need(input$numeric_var %in% names(df), "Variable numérique indisponible."))

      plotly::plot_ly(
        data = df,
        x = df[[input$numeric_var]],
        color = ~claim_status,
        type = "histogram",
        nbinsx = 40,
        alpha = 0.75
      ) %>%
        plotly::layout(
          barmode = "overlay",
          xaxis = list(title = input$numeric_var),
          yaxis = list(title = "Nombre de dossiers")
        )
    })

    output$categorical_summary <- DT::renderDT({
      df <- filtered_data()
      req(input$categorical_var)
      validate(need(input$categorical_var %in% names(df), "Variable catégorielle indisponible."))

      summary <- df %>%
        mutate(category = as.character(.data[[input$categorical_var]])) %>%
        count(category, claim_status, name = "n") %>%
        group_by(category) %>%
        mutate(total = sum(n), share = n / sum(n)) %>%
        ungroup() %>%
        arrange(desc(total), category, claim_status) %>%
        head(80)

      DT::datatable(round_numeric(summary), rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })

    output$categorical_distribution <- plotly::renderPlotly({
      df <- filtered_data()
      req(input$categorical_var)
      validate(need(input$categorical_var %in% names(df), "Variable catégorielle indisponible."))

      plot_data <- df %>%
        mutate(category = as.character(.data[[input$categorical_var]])) %>%
        count(category, sort = TRUE) %>%
        slice_head(n = 15) %>%
        arrange(n)

      plotly::plot_ly(plot_data, x = ~n, y = ~category, type = "bar", orientation = "h") %>%
        plotly::layout(xaxis = list(title = "Nombre de dossiers"), yaxis = list(title = ""))
    })

    output$target_distribution <- plotly::renderPlotly({
      target <- filtered_data() %>% count(claim_status, name = "n")
      plotly::plot_ly(target, x = ~claim_status, y = ~n, type = "bar") %>%
        plotly::layout(xaxis = list(title = "Claim Status"), yaxis = list(title = "Nombre de dossiers"))
    })

    claim_rate_data <- reactive({
      df <- filtered_data()
      req(input$rate_var)
      validate(need(input$rate_var %in% names(df), "Variable d'analyse indisponible."))

      variable <- input$rate_var
      if (variable %in% c("age", "duration", "net_sales")) {
        values <- df[[variable]]
        breaks <- unique(stats::quantile(values, probs = seq(0, 1, 0.1), na.rm = TRUE))
        if (length(breaks) < 3) {
          df$bucket <- as.character(values)
        } else {
          df$bucket <- cut(values, breaks = breaks, include.lowest = TRUE, dig.lab = 8)
        }
      } else {
        df$bucket <- as.character(df[[variable]])
      }

      df %>%
        filter(!is.na(bucket)) %>%
        group_by(bucket) %>%
        summarise(
          n = n(),
          claims = sum(claim_status == "yes"),
          claim_rate = claims / n,
          .groups = "drop"
        ) %>%
        filter(n >= 20) %>%
        arrange(desc(claim_rate), desc(n))
    })

    output$claim_rate_table <- DT::renderDT({
      DT::datatable(round_numeric(claim_rate_data()), rownames = FALSE, options = list(pageLength = 12, scrollX = TRUE))
    })

    output$claim_rate_plot <- plotly::renderPlotly({
      plot_data <- claim_rate_data() %>% slice_head(n = 20) %>% arrange(claim_rate)
      validate(need(nrow(plot_data) > 0, "Aucune modalité avec suffisamment d'observations."))

      plotly::plot_ly(plot_data, x = ~claim_rate, y = ~bucket, type = "bar", orientation = "h") %>%
        plotly::layout(
          xaxis = list(title = "Taux de sinistre", tickformat = ".1%"),
          yaxis = list(title = "")
        )
    })
  })
}
