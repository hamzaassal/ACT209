# call_llm_agent.R
# Appel optionnel a un LLM, avec fallback local robuste.

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || anyNA(x)) y else x
}

local_agent_fallback <- function(user_question, client_data, prediction_result) {
  recommendation <- generate_recommendation(
    prediction_result$probability_yes,
    prediction_result$risk_segment,
    prediction_result$segment_lift,
    prediction_result$segment_claim_rate
  )

  paste0(
    "Reponse locale de secours generee a partir du score, du segment et des regles metier du projet.\n\n",
    "Question : ", user_question, "\n\n",
    "1. Synthese du risque\n",
    "Le dossier presente un score de sinistre estime a ",
    round(prediction_result$probability_yes * 100, 2), " %. ",
    "Il est classe dans le segment ", prediction_result$risk_segment,
    " (", prediction_result$segment_condition, "), avec un lift de ",
    round(prediction_result$segment_lift, 2), " par rapport au taux moyen du portefeuille.\n\n",
    "2. Elements explicatifs\n",
    explain_prediction(prediction_result, client_data), "\n\n",
    "3. Recommandation underwriting\n",
    recommendation, "\n\n",
    "4. Points de vigilance\n",
    "Le score doit etre compare aux informations du dossier et aux regles internes de souscription. ",
    "Un score eleve indique une priorite d'analyse, pas une certitude de sinistre.\n\n",
    "5. Limites du modele\n",
    "Le modele est entraine sur une base historique et ne prouve pas qu'un sinistre aura lieu. ",
    "Il ne remplace pas l'underwriter, ne constitue pas une decision juridique ou contractuelle, ",
    "et doit etre utilise comme outil d'aide a la priorisation."
  )
}

call_llm_agent <- function(user_question, client_data, prediction_result) {
  context <- build_agent_context(client_data, prediction_result)
  project_root <- Sys.getenv("TRAVEL_INSURANCE_PROJECT_ROOT", unset = "")
  candidates <- c(
    if (nzchar(project_root)) file.path(project_root, "agent", "agent_prompt.md"),
    file.path("agent", "agent_prompt.md"),
    file.path("..", "agent", "agent_prompt.md"),
    file.path(dirname(sys.frame(1)$ofile %||% "."), "agent_prompt.md")
  )
  prompt_path <- candidates[file.exists(candidates)][1]

  system_prompt <- if (!is.na(prompt_path) && file.exists(prompt_path)) {
    paste(readLines(prompt_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  } else {
    "Tu es un assistant IA underwriting assurance voyage. Reponds en francais clair, sans decision automatique."
  }

  fallback_response <- local_agent_fallback(user_question, client_data, prediction_result)

  api_key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  if (!nzchar(api_key)) {
    return(paste0(
      "Mode fallback : aucune cle API LLM detectee.\n\n",
      fallback_response
    ))
  }

  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("jsonlite", quietly = TRUE)) {
    return(paste0(
      "Mode fallback : packages httr/jsonlite indisponibles pour l'appel LLM.\n\n",
      fallback_response
    ))
  }

  endpoint <- Sys.getenv("OPENAI_API_ENDPOINT", unset = "https://api.openai.com/v1/chat/completions")
  model <- Sys.getenv("OPENAI_MODEL", unset = "gpt-4o-mini")

  body <- list(
    model = model,
    messages = list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = paste0(context, "\n\nQuestion utilisateur : ", user_question))
    ),
    temperature = 0.2
  )

  tryCatch(
    {
      res <- httr::POST(
        endpoint,
        httr::add_headers(
          Authorization = paste("Bearer", api_key),
          `Content-Type` = "application/json"
        ),
        body = jsonlite::toJSON(body, auto_unbox = TRUE),
        encode = "raw",
        httr::timeout(30)
      )

      if (httr::http_error(res)) {
        api_message <- tryCatch({
          parsed_error <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
          parsed_error$error$message %||% paste("code HTTP", httr::status_code(res))
        }, error = function(e) paste("code HTTP", httr::status_code(res)))
        stop("Erreur API LLM : ", api_message)
      }

      parsed <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
      parsed$choices[[1]]$message$content
    },
    error = function(e) {
      paste0(
        "Mode fallback : l'appel LLM a echoue (", conditionMessage(e), ").\n\n",
        fallback_response
      )
    }
  )
}
