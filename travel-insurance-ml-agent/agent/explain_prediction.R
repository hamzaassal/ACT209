# explain_prediction.R
# Explication locale déterministe du score de sinistre.

explain_prediction <- function(prediction_result, client_data) {
  if (missing(prediction_result) || missing(client_data)) {
    stop("prediction_result et client_data sont obligatoires.")
  }

  probability <- prediction_result$probability_yes
  baseline <- prediction_result$baseline_claim_rate
  lift <- prediction_result$segment_lift
  segment <- prediction_result$risk_segment
  claim_rate <- prediction_result$segment_claim_rate

  explanation <- paste0(
    "Le modèle estime une probabilité de sinistre de ",
    round(probability * 100, 2), " %. ",
    "Le taux moyen de sinistre du portefeuille de référence est d'environ ",
    round(baseline * 100, 2), " %. ",
    "Le dossier est classé dans le segment de risque '", segment, "', ",
    "associé à un taux historique de sinistre de ",
    round(claim_rate * 100, 2), " % et à un lift de ",
    round(lift, 2), ". ",
    "Cela signifie que ce segment est plus sinistré que la moyenne du portefeuille. ",
    "Cette sortie doit être utilisée comme un score de priorisation, pas comme une décision automatique."
  )

  if (!is.null(prediction_result$important_features)) {
    explanation <- paste0(
      explanation,
      "\n\nVariables importantes disponibles : ",
      paste(prediction_result$important_features, collapse = ", "), "."
    )
  } else {
    explanation <- paste0(
      explanation,
      "\n\nL'interprétation fine variable par variable pourra être enrichie ultérieurement avec des valeurs SHAP. ",
      "Aucune explication SHAP locale n'est actuellement disponible dans les métadonnées du modèle."
    )
  }

  explanation
}
