# modules/mod_input_client.R
# Module de saisie dossier.

mod_input_client_ui <- function(id) {
  ns <- NS(id)
  card(
    card_header("Caractéristiques du dossier"),
    layout_columns(
      col_widths = c(6, 6),
      selectInput(ns("agency"), "Agence", choices = input_choices$agency, selected = "C2B"),
      selectInput(ns("agency_type"), "Type d'agence", choices = input_choices$agency_type, selected = "Airlines"),
      selectInput(ns("distribution_channel"), "Canal de distribution", choices = input_choices$distribution_channel, selected = "Online"),
      selectInput(ns("product_name"), "Produit", choices = input_choices$product_name, selected = "Cancellation Plan"),
      numericInput(ns("duration"), "Durée du voyage", value = 14, min = 0, max = 3650),
      selectInput(ns("destination"), "Destination", choices = input_choices$destination, selected = if ("SINGAPORE" %in% input_choices$destination) "SINGAPORE" else input_choices$destination[[1]]),
      numericInput(ns("net_sales"), "Net sales", value = 50, min = -500, max = 1000),
      numericInput(ns("commision_in_value"), "Commission", value = 10, min = 0, max = 500),
      selectInput(ns("gender"), "Genre", choices = input_choices$gender, selected = if ("F" %in% input_choices$gender) "F" else input_choices$gender[[1]]),
      numericInput(ns("age"), "Âge", value = 40, min = 0, max = 120)
    ),
    actionButton(ns("calculate"), "Calculer le score de risque", class = "btn-primary")
  )
}

mod_input_client_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    reactive({
      list(
        calculate = input$calculate,
        data = data.frame(
          agency = input$agency,
          agency_type = input$agency_type,
          distribution_channel = input$distribution_channel,
          product_name = input$product_name,
          duration = input$duration,
          destination = input$destination,
          net_sales = input$net_sales,
          commision_in_value = input$commision_in_value,
          gender = input$gender,
          age = input$age,
          stringsAsFactors = FALSE
        )
      )
    })
  })
}
