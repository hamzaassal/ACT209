# ui.R
# Interface principale de l'application underwriting.

app_ui <- page_navbar(
  title = "Travel Insurance Risk Scoring",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#1f4e79",
    base_font = "Arial"
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  nav_panel(
    "Accueil",
    layout_columns(
      col_widths = c(7, 5),
      card(
        card_header("Objectif du prototype"),
        p("Cette application estime un score de risque de sinistre pour un dossier d'assurance voyage."),
        p("Le score est produit par le modèle champion sauvegardé et doit être interprété comme une aide à la décision underwriting."),
        tags$div(
          class = "warning-box",
          strong("Important : "),
          "l'outil ne prend aucune décision automatique et ne remplace pas l'analyse de l'underwriter."
        )
      ),
      card(
        card_header("Logique scoring/ranking"),
        p("Le modèle trie les dossiers selon leur probabilité estimée de sinistre."),
        tags$ul(
          tags$li("Top 1 % : segment des scores les plus élevés."),
          tags$li("Top 5 % et Top 10 % : segments utiles pour prioriser les revues underwriting."),
          tags$li("Top 20 % : vision élargie des dossiers à surveiller."),
          tags$li("Les taux et lifts détaillés sont disponibles dans les onglets Exploration statistique et Synthèse Machine Learning.")
        )
      )
    )
  ),

  nav_panel(
    "Exploration statistique",
    mod_stat_exploration_ui("stats")
  ),

  nav_panel(
    "Synthèse Machine Learning",
    mod_ml_summary_ui("ml")
  ),

  nav_panel(
    "Scoring client",
    mod_client_scoring_ui("scoring")
  ),

  nav_panel(
    "Agent IA",
    mod_agent_chat_ui("agent")
  ),

  nav_panel(
    "Documentation",
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Documents utilisateur"),
        tags$ul(
          tags$li(tags$a(href = "../docs/guide_utilisation_application.md", "Guide d'utilisation application")),
          tags$li(tags$a(href = "../docs/procedure_creation_agent_ia.md", "Procédure de création de l'agent IA")),
          tags$li(tags$a(href = "../docs/architecture_modele_shiny_agent.md", "Architecture modèle - Shiny - agent"))
        ),
        p("Ces fichiers sont disponibles dans le dossier docs/ du projet.")
      ),
      card(
        card_header("Précautions d'usage"),
        tags$ul(
          tags$li("Le modèle estime une probabilité, pas une certitude."),
          tags$li("Le score priorise les dossiers, il ne décide pas."),
          tags$li("Les règles internes de souscription restent prioritaires."),
          tags$li("L'agent IA explique et contextualise ; il ne modifie pas le modèle.")
        )
      )
    )
  )
)
