# modules/mod_input_client.R
# Module de saisie dossier.

mod_input_client_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      class = "section-intro scoring-intro",
      h2("Scoring d'un dossier"),
      p("Renseignez les informations du dossier afin d'obtenir une estimation du risque de sinistre. Les variables tarifaires deja calculees ne sont pas demandees dans cette version applicative.")
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(
        class = "content-card input-card",
        card_header("Informations generales du voyage"),
        selectInput(ns("agency"), "Agence / partenaire", choices = input_choices$agency, selected = select_existing(input_choices$agency, "C2B")),
        selectInput(ns("agency_type"), "Type d'agence", choices = input_choices$agency_type, selected = select_existing(input_choices$agency_type, "Airlines")),
        selectInput(ns("distribution_channel"), "Canal de distribution", choices = input_choices$distribution_channel, selected = select_existing(input_choices$distribution_channel, "Online")),
        selectInput(ns("destination"), "Destination", choices = input_choices$destination, selected = select_existing(input_choices$destination, "SINGAPORE"))
      ),
      card(
        class = "content-card input-card",
        card_header("Police et profil assure"),
        selectInput(ns("product_name"), "Produit d'assurance", choices = input_choices$product_name, selected = select_existing(input_choices$product_name, "Cancellation Plan")),
        numericInput(ns("duration"), "Duree du voyage (jours)", value = 14, min = 0, max = 3650, step = 1),
        selectInput(ns("gender"), "Genre declare", choices = input_choices$gender, selected = select_existing(input_choices$gender, "F")),
        numericInput(ns("age"), "Age du voyageur", value = 40, min = 0, max = 120, step = 1)
      )
    ),
    card(
      class = "content-card action-card",
      div(
        class = "action-copy",
        strong("Modele applicatif underwriting"),
        span("La recommandation ne s'appuie pas sur une prime ou une commission deja calculee.")
      ),
      div(
        class = "form-actions",
        actionButton(ns("reset"), "Reinitialiser le formulaire", class = "btn btn-outline-secondary"),
        actionButton(ns("calculate"), "Calculer le score de risque", class = "btn btn-primary btn-lg")
      )
    )
  )
}

mod_input_client_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$reset, {
      updateSelectInput(session, "agency", selected = select_existing(input_choices$agency, "C2B"))
      updateSelectInput(session, "agency_type", selected = select_existing(input_choices$agency_type, "Airlines"))
      updateSelectInput(session, "distribution_channel", selected = select_existing(input_choices$distribution_channel, "Online"))
      updateSelectInput(session, "product_name", selected = select_existing(input_choices$product_name, "Cancellation Plan"))
      updateNumericInput(session, "duration", value = 14)
      updateSelectInput(session, "destination", selected = select_existing(input_choices$destination, "SINGAPORE"))
      updateSelectInput(session, "gender", selected = select_existing(input_choices$gender, "F"))
      updateNumericInput(session, "age", value = 40)
    })

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
          gender = input$gender,
          age = input$age,
          stringsAsFactors = FALSE
        )
      )
    })
  })
}
