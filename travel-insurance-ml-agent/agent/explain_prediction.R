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

  if (!is.null(prediction_result$shap_top_features) && nrow(prediction_result$shap_top_features) > 0) {
    shap_lines <- paste0(
      prediction_result$shap_top_features$feature_group,
      " (",
      prediction_result$shap_top_features$direction,
      ")",
      collapse = ", "
    )
    explanation <- paste0(
      explanation,
      "\n\nPrincipales contributions SHAP locales pour ce dossier : ",
      shap_lines,
      ". Ces contributions indiquent les groupes de variables qui influencent le plus le score du dossier, à la hausse ou à la baisse."
    )
  } else if (!is.null(prediction_result$important_features)) {
    explanation <- paste0(
      explanation,
      "\n\nVariables importantes disponibles : ",
      paste(prediction_result$important_features, collapse = ", "), "."
    )
  } else {
    explanation <- paste0(
      explanation,
      "\n\nLes contributions SHAP locales ne sont pas disponibles pour ce dossier. ",
      "L'interprétation reste donc limitée à la lecture du score, du segment et du lift."
    )
  }

  explanation
}
