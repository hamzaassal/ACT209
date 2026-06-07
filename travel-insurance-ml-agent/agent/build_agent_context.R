# build_agent_context.R
# Construction du contexte transmis a l'agent IA.

build_agent_context <- function(client_data, prediction_result) {
  client_lines <- paste(
    paste0("- ", names(client_data), " : ", as.character(client_data[1, , drop = TRUE])),
    collapse = "\n"
  )

  removed_tariff_variables <- prediction_result$removed_tariff_variables %||% character(0)
  removed_tariff_text <- if (length(removed_tariff_variables) > 0) {
    paste(removed_tariff_variables, collapse = ", ")
  } else {
    "aucune information disponible"
  }

  paste0(
    "CONTEXTE DOSSIER ASSURANCE VOYAGE\n\n",
    "Caracteristiques du dossier :\n",
    client_lines,
    "\n\nResultat du modele :\n",
    "- Modele utilise : ", prediction_result$model_label %||% "modele applicatif", "\n",
    "- Positionnement methodologique : ", prediction_result$model_source %||% "non renseigne", "\n",
    "- Variables tarifaires exclues : ", removed_tariff_text, "\n",
    "- Probabilite estimee de sinistre : ", round(prediction_result$probability_yes * 100, 2), " %\n",
    "- Seuil operationnel utilise : ", round(prediction_result$threshold_used * 100, 2), " %\n",
    "- Classe predite au seuil : ", prediction_result$predicted_class, "\n",
    "- Segment de risque : ", prediction_result$risk_segment, " (", prediction_result$segment_condition, ")\n",
    "- Taux de sinistre historique du segment : ", round(prediction_result$segment_claim_rate * 100, 2), " %\n",
    "- Lift du segment : ", round(prediction_result$segment_lift, 2), "\n",
    "- Taux moyen de sinistre du portefeuille : ", round(prediction_result$baseline_claim_rate * 100, 2), " %\n",
    "- Recommandation automatique : ", prediction_result$recommendation, "\n\n",
    "Limites a rappeler :\n",
    "- Le score est probabiliste.\n",
    "- Le modele ne prouve pas qu'un sinistre aura lieu.\n",
    "- Le modele ne remplace pas l'analyse de l'underwriter.\n",
    "- La recommandation applicative ne repose pas sur une prime deja calculee.\n",
    "- Les regles internes de souscription restent prioritaires.\n"
  )
}
