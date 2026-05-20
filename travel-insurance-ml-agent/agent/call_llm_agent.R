# call_llm_agent.R
# Appel optionnel à un LLM, avec fallback local robuste si aucune clé API
# n'est disponible ou si l'appel externe échoue.

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
    "Tu es un assistant IA underwriting assurance voyage. Réponds en français clair, sans décision automatique."
  }

  fallback_response <- paste0(
    "Mode fallback : aucune clé API LLM détectée, réponse générée localement.\n\n",
    "Question : ", user_question, "\n\n",
    "1. Synthèse du risque\n",
    explain_prediction(prediction_result, client_data), "\n\n",
    "2. Recommandation underwriting\n",
    generate_recommendation(
      prediction_result$probability_yes,
      prediction_result$risk_segment,
      prediction_result$segment_lift,
      prediction_result$segment_claim_rate
    ), "\n\n",
    "3. Points de vigilance\n",
    "Ce score est une aide à la décision. Il doit être complété par l'analyse humaine, les règles internes et les informations contractuelles disponibles."
  )

  api_key <- Sys.getenv("OPENAI_API_KEY", unset = "")
  if (!nzchar(api_key)) {
    return(fallback_response)
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

  response <- tryCatch(
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
        stop("Erreur API LLM : ", httr::status_code(res))
      }

      parsed <- jsonlite::fromJSON(httr::content(res, as = "text", encoding = "UTF-8"), simplifyVector = FALSE)
      parsed$choices[[1]]$message$content
    },
    error = function(e) {
      paste0(
        "Mode fallback : l'appel LLM a échoué (", conditionMessage(e), ").\n\n",
        fallback_response
      )
    }
  )

  response
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}
