# app.R
# Point d'entrée Shiny. global.R est chargé automatiquement par Shiny.

source("ui.R", local = TRUE)
source("server.R", local = TRUE)

shinyApp(ui = app_ui, server = app_server)
