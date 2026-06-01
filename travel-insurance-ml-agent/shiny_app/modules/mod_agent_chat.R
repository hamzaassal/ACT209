# modules/mod_agent_chat.R
# Module conversationnel avec l'agent IA underwriting.

mod_agent_chat_ui <- function(id) {
  ns <- NS(id)
  layout_columns(
    col_widths = c(4, 8),
    card(
      class = "content-card agent-context-card",
      card_header("Contexte agent"),
      uiOutput(ns("context_summary")),
      tags$hr(),
      p(class = "card-help", "Questions rapides"),
      actionButton(ns("q_risk"), "Pourquoi ce dossier est-il risque ?", class = "btn btn-outline-primary question-btn"),
      actionButton(ns("q_reco"), "Quelle recommandation underwriting ?", class = "btn btn-outline-primary question-btn"),
      actionButton(ns("q_lift"), "Que signifie le lift ?", class = "btn btn-outline-primary question-btn"),
      actionButton(ns("q_limits"), "Quelles sont les limites du modele ?", class = "btn btn-outline-primary question-btn"),
      actionButton(ns("q_average"), "Comment interpreter ce score face au taux moyen ?", class = "btn btn-outline-primary question-btn"),
      div(
        class = "info-box compact",
        if (nzchar(Sys.getenv("OPENAI_API_KEY", unset = ""))) {
          "Mode LLM actif : une cle API est detectee."
        } else {
          "Mode local : reponse generee sans appel externe a un LLM."
        }
      )
    ),
    card(
      class = "content-card agent-main-card",
      card_header("Agent IA explicatif"),
      textAreaInput(
        ns("question"),
        "Question pour l'agent",
        value = "Explique le niveau de risque de ce dossier et propose une recommandation underwriting.",
        rows = 4
      ),
      div(
        class = "form-actions",
        actionButton(ns("ask"), "Interroger l'agent", class = "btn btn-primary")
      ),
      tags$hr(),
      uiOutput(ns("answer_panel"))
    )
  )
}

mod_agent_chat_server <- function(id, client_data, prediction_result, prediction_error = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$q_risk, {
      updateTextAreaInput(session, "question", value = "Pourquoi ce dossier est-il risque ?")
    })
    observeEvent(input$q_reco, {
      updateTextAreaInput(session, "question", value = "Quelle recommandation underwriting proposes-tu ?")
    })
    observeEvent(input$q_lift, {
      updateTextAreaInput(session, "question", value = "Que signifie le lift pour ce dossier ?")
    })
    observeEvent(input$q_limits, {
      updateTextAreaInput(session, "question", value = "Quelles sont les limites du modele et de cette recommandation ?")
    })
    observeEvent(input$q_average, {
      updateTextAreaInput(session, "question", value = "Comment interpreter ce score face au taux moyen du portefeuille ?")
    })

    output$context_summary <- renderUI({
      if (!is.null(prediction_error())) {
        return(div(class = "alert-box compact", paste("Erreur de scoring :", prediction_error())))
      }

      result <- prediction_result()
      if (is.null(result)) {
        return(div(class = "empty-mini", "Calculez un score pour alimenter le contexte de l'agent."))
      }

      shap_items <- NULL
      if (!is.null(result$shap_top_features) && nrow(result$shap_top_features) > 0) {
        shap_top <- utils::head(result$shap_top_features, 3)
        shap_items <- tags$ul(
          class = "shap-list",
          lapply(seq_len(nrow(shap_top)), function(i) {
            direction <- shap_top$direction[i] %||% ifelse(shap_top$shap_value[i] >= 0, "Augmente le score", "Diminue le score")
            tags$li(paste0(shap_top$feature_group[i], " : ", tolower(direction)))
          })
        )
      } else {
        shap_items <- tags$p(
          class = "technical-note",
          "Interpretation locale SHAP indisponible pour ce dossier."
        )
      }

      tagList(
        div(
          class = "context-kpis",
          div(tags$span("Score"), strong(format_probability(result$probability_yes))),
          div(tags$span("Segment"), strong(result$risk_level)),
          div(tags$span("Lift"), strong(paste0("x", round(result$segment_lift, 2))))
        ),
        div(
          class = "shap-mini",
          tags$strong("Principaux facteurs locaux transmis a l'agent"),
          shap_items
        )
      )
    })

    answer <- eventReactive(input$ask, {
      if (!is.null(prediction_error())) {
        return(paste("Agent indisponible : le scoring contient une erreur -", prediction_error()))
      }
      if (is.null(prediction_result())) {
        return("Veuillez d'abord calculer le score du dossier avant d'interroger l'agent IA.")
      }
      if (!nzchar(trimws(input$question %||% ""))) {
        return("Veuillez saisir une question pour l'agent.")
      }

      call_llm_agent(input$question, client_data()$data, prediction_result())
    })

    output$answer_panel <- renderUI({
      if (is.null(answer())) {
        return(empty_state(
          "Agent en attente",
          "Calculez un score puis interrogez l'agent pour obtenir une explication structuree."
        ))
      }

      div(
        class = "agent-answer",
        tags$pre(answer())
      )
    })
  })
}
