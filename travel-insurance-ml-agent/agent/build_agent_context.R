# build_agent_context.R
# Construction du contexte transmis à l'agent IA.

build_agent_context <- function(client_data, prediction_result) {
  client_lines <- paste(
    paste0("- ", names(client_data), " : ", as.character(client_data[1, , drop = TRUE])),
    collapse = "\n"
  )

  paste0(
    "CONTEXTE DOSSIER ASSURANCE VOYAGE\n\n",
    "Caractéristiques du dossier :\n",
    client_lines,
    "\n\nRésultat du modèle :\n",
    "- Probabilité estimée de sinistre : ", round(prediction_result$probability_yes * 100, 2), " %\n",
    "- Seuil opérationnel utilisé : ", round(prediction_result$threshold_used * 100, 2), " %\n",
    "- Classe prédite au seuil : ", prediction_result$predicted_class, "\n",
    "- Segment de risque : ", prediction_result$risk_segment, " (", prediction_result$segment_condition, ")\n",
    "- Taux de sinistre historique du segment : ", round(prediction_result$segment_claim_rate * 100, 2), " %\n",
    "- Lift du segment : ", round(prediction_result$segment_lift, 2), "\n",
    "- Taux moyen de sinistre du portefeuille : ", round(prediction_result$baseline_claim_rate * 100, 2), " %\n",
    "- Recommandation automatique : ", prediction_result$recommendation_level, "\n\n",
    "Limites à rappeler :\n",
    "- Le score est probabiliste.\n",
    "- Le modèle ne prouve pas qu'un sinistre aura lieu.\n",
    "- Le modèle ne remplace pas l'analyse de l'underwriter.\n",
    "- Les règles internes de souscription restent prioritaires.\n"
  )
}
