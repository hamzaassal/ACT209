# ui.R
# Interface Shiny professionnelle pour le prototype underwriting.

brand_title <- tags$div(
  class = "navbar-brand-content",
  logo_block(compact = TRUE),
  tags$div(
    class = "navbar-title-stack",
    tags$span(class = "nav-title-main", "Risk Scoring"),
    tags$span(class = "nav-title-accent", "& Agent IA")
  )
)

home_card <- function(number, title, text) {
  div(
    class = "value-card",
    span(class = "value-number", number),
    div(
      class = "value-copy",
      strong(title),
      p(text)
    )
  )
}

process_step <- function(number, title, text) {
  div(
    class = "process-step",
    span(class = "process-number", number),
    div(
      class = "process-copy",
      strong(title),
      p(text)
    )
  )
}

roadmap_row <- function(step, input, model_action, output, agent_role) {
  tags$tr(
    tags$td(span(class = "roadmap-step", step)),
    tags$td(input),
    tags$td(model_action),
    tags$td(output),
    tags$td(agent_role)
  )
}

home_metric <- function(label, value, detail, class = "") {
  div(
    class = paste("home-metric", class),
    span(label),
    strong(value),
    tags$small(detail)
  )
}

method_item <- function(title, text) {
  div(class = "method-item", strong(title), span(text))
}

app_ui <- page_navbar(
  title = brand_title,
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#234f7d",
    secondary = "#64748b",
    base_font = "Arial",
    heading_font = "Arial"
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  nav_panel(
    "Accueil",
    div(
      class = "page-shell home-page",
      div(
        class = "home-board",
        div(
          class = "home-identity-panel",
          div(class = "eyebrow", "Master 2 Actuariat - Assurance voyage"),
          div(
            class = "product-name",
            span(class = "product-main", "Risk Scoring"),
            span(class = "product-accent", "& Agent IA")
          ),
          div(
            class = "home-subtitle-line",
            "Scoring du risque de sinistre en assurance voyage"
          ),
          div(
            class = "home-tagline",
            "Prototype underwriter-ready : score, segment, recommandation et explication IA."
          )
        ),
        div(
          class = "home-status-panel",
          home_metric("Modele applicatif", "XGBoost", "Sans variables tarifaires", "metric-blue"),
          home_metric("Sortie", "Score", "Probabilite estimee de sinistre", "metric-green"),
          home_metric("Usage", "Aide", "Priorisation, pas decision automatique", "metric-amber")
        )
      ),
      div(
        class = "roadmap-card",
        div(
          class = "roadmap-header",
          h2("Roadmap d'utilisation"),
          span("Dossier client -> score de risque -> segment -> agent IA")
        ),
        tags$table(
          class = "roadmap-table",
          tags$thead(
            tags$tr(
              tags$th("Etape"),
              tags$th("Entree"),
              tags$th("Traitement"),
              tags$th("Sortie visible"),
              tags$th("Role agent IA")
            )
          ),
          tags$tbody(
            roadmap_row("01", "Caracteristiques du dossier", "Controle des champs et preparation modele", "Dossier pret au scoring", "Contexte client disponible"),
            roadmap_row("02", "Dossier valide", "Prediction XGBoost sans prime ni commission", "Probabilite de sinistre", "Traduction du score en langage metier"),
            roadmap_row("03", "Score calcule", "Segmentation risque et lift", "Niveau d'attention recommande", "Explication des facteurs et limites"),
            roadmap_row("04", "Question underwriter", "Contexte + prompt agent", "Reponse structuree", "Recommandation assistée, non automatique")
          )
        )
      ),
      div(
        class = "home-command-grid",
        div(class = "command-card", span("1"), strong("Saisir"), tags$small("Informations pre-tarification")),
        div(class = "command-card", span("2"), strong("Scorer"), tags$small("Probabilite de sinistre")),
        div(class = "command-card", span("3"), strong("Prioriser"), tags$small("Segment, lift, niveau d'attention")),
        div(class = "command-card command-agent", span("4"), strong("Expliquer"), tags$small("Agent IA pour l'underwriter"))
      ),
      div(
        class = "home-principle",
        strong("Principe d'usage"),
        span("L'outil accompagne l'analyse. La decision finale reste du ressort de l'underwriter et des regles internes.")
      )
    )
  ),

  nav_panel(
    "Scoring d'un dossier",
    div(
      class = "page-shell wide-page",
      mod_input_client_ui("client")
    )
  ),

  nav_panel(
    "Analyse du risque",
    div(
      class = "page-shell wide-page",
      mod_risk_analysis_ui("risk_analysis")
    )
  ),

  nav_panel(
    "Agent IA explicatif",
    div(
      class = "page-shell wide-page",
      div(
        class = "section-intro",
        h2("Agent IA explicatif"),
        p("L'agent explique le score du modele, le segment de risque et la recommandation. Il ne remplace pas l'analyse de l'underwriter.")
      ),
      mod_agent_chat_ui("agent")
    )
  ),

  nav_panel(
    "Methodologie & limites",
    div(
      class = "page-shell wide-page",
      layout_columns(
        col_widths = c(7, 5),
        card(
          class = "content-card",
          card_header("Methodologie du scoring"),
          method_item("Donnees", "Dataset Kaggle Travel Insurance, cible claim_status."),
          method_item("Objectif", "Estimer la probabilite de sinistre, classe yes."),
          method_item("Modele applicatif", "XGBoost sans variables tarifaires afin d'eviter une logique circulaire avec la prime."),
          method_item("Usage", "Scoring et ranking du risque, pas decision automatique."),
          method_item("Seuil", "Seuil operationnel charge depuis les metadonnees du modele applicatif.")
        ),
        card(
          class = "content-card",
          card_header("Limites a rappeler"),
          div(class = "limit-list",
            p("Le modele n'a pas vocation a predire avec certitude la survenance d'un sinistre."),
            p("L'accuracy est insuffisante dans un contexte de sinistres rares."),
            p("Les faux positifs et faux negatifs doivent etre interpretes selon les couts metier."),
            p("Les performances doivent etre surveillees si le portefeuille evolue ou si les donnees derivent."),
            p("La decision finale reste du ressort de l'underwriter.")
          )
        )
      )
    )
  ),

  nav_panel(
    "Guide utilisateur",
    div(
      class = "page-shell wide-page",
      layout_columns(
        col_widths = c(6, 6),
        card(
          class = "content-card",
          card_header("Etapes d'utilisation"),
          tags$ol(
            tags$li("Ouvrir Scoring d'un dossier et renseigner les informations disponibles avant tarification."),
            tags$li("Cliquer sur Calculer le score de risque."),
            tags$li("Consulter Analyse du risque pour lire le score, le segment, le lift et la recommandation."),
            tags$li("Interroger l'agent IA pour obtenir une explication metier structuree.")
          )
        ),
        card(
          class = "content-card",
          card_header("Lire les indicateurs"),
          method_item("Vert", "Aucune alerte particuliere selon le modele."),
          method_item("Bleu / jaune", "Signal de vigilance ou revue standard recommandee."),
          method_item("Orange / rouge", "Analyse complementaire ou prioritaire avant decision."),
          method_item("Exemple Top 10 %", "Un dossier Top 10 % appartient aux 10 % de scores les plus eleves ; ce segment concentrait environ 53,76 % des sinistres dans le test set du modele complet.")
        )
      ),
      card(
        class = "content-card",
        card_header("Documentation"),
        p(tags$a(href = "docs/guide_utilisation_application.md", target = "_blank", "Guide utilisateur complet")),
        p(tags$a(href = "docs/procedure_creation_agent_ia.md", target = "_blank", "Procedure de creation de l'agent IA")),
        div(class = "info-box", "Bonne pratique : utiliser le score pour prioriser l'analyse, puis documenter la decision selon les regles internes.")
      )
    )
  )
)
