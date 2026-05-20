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
        p("Le score est produit par le modèle champion XGBoost sans SMOTE et doit être interprété comme une aide à la décision underwriting."),
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
          tags$li("Top 1 % : taux de sinistre 16,22 %, lift 9,64"),
          tags$li("Top 5 % : 32,80 % des sinistres capturés, lift 6,56"),
          tags$li("Top 10 % : 53,76 % des sinistres capturés, lift 5,37"),
          tags$li("Top 20 % : 70,43 % des sinistres capturés, lift 3,52")
        )
      )
    )
  ),

  nav_panel(
    "Saisie dossier",
    layout_columns(
      col_widths = c(5, 7),
      mod_input_client_ui("client"),
      card(
        card_header("Aide de saisie"),
        p("Saisissez les caractéristiques du dossier telles qu'elles sont attendues par le modèle."),
        p("Les valeurs par défaut sont réalistes et servent uniquement à tester l'application."),
        tags$div(
          class = "info-box",
          "Après calcul, consultez les onglets Résultat scoring, Analyse métier et Agent IA."
        )
      )
    )
  ),

  nav_panel(
    "Résultat scoring",
    mod_score_output_ui("score")
  ),

  nav_panel(
    "Analyse métier",
    mod_risk_explanation_ui("risk")
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
