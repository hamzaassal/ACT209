# modules/mod_agent_chat.R
# Module conversationnel avec l'agent IA underwriting.

mod_agent_chat_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(4, 8),
    card(
      card_header("Questions prédéfinies"),
      actionButton(ns("q_risk"), "Pourquoi ce dossier est-il risqué ?", class = "btn-outline-primary question-btn"),
      actionButton(ns("q_reco"), "Quelle recommandation underwriting ?", class = "btn-outline-primary question-btn"),
      actionButton(ns("q_lift"), "Que signifie le lift ?", class = "btn-outline-primary question-btn"),
      actionButton(ns("q_limits"), "Quelles sont les limites du modèle ?", class = "btn-outline-primary question-btn")
    ),
    card(
      card_header("Interroger l'agent IA"),
      textAreaInput(ns("question"), "Question", value = "Pourquoi ce dossier mérite-t-il une analyse underwriting ?", rows = 4),
      actionButton(ns("ask"), "Interroger l'agent", class = "btn-primary"),
      tags$hr(),
      verbatimTextOutput(ns("answer"))
    )
  )
}

mod_agent_chat_server <- function(id, client_data, prediction_result) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$q_risk, {
      updateTextAreaInput(session, "question", value = "Pourquoi ce dossier est-il risqué ?")
    })
    observeEvent(input$q_reco, {
      updateTextAreaInput(session, "question", value = "Quelle recommandation underwriting proposes-tu ?")
    })
    observeEvent(input$q_lift, {
      updateTextAreaInput(session, "question", value = "Que signifie le lift dans ce dossier ?")
    })
    observeEvent(input$q_limits, {
      updateTextAreaInput(session, "question", value = "Quelles sont les limites du modèle pour ce dossier ?")
    })

    answer <- eventReactive(input$ask, {
      req(client_data()$data, prediction_result(), input$question)
      call_llm_agent(input$question, client_data()$data, prediction_result())
    })

    output$answer <- renderText({
      req(answer())
      answer()
    })
  })
}
