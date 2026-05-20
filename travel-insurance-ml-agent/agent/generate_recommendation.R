# generate_recommendation.R
# Recommandation underwriting déterministe utilisée aussi en fallback LLM.

generate_recommendation <- function(probability, risk_segment, lift, claim_rate) {
  probability <- as.numeric(probability)
  lift <- as.numeric(lift)
  claim_rate <- as.numeric(claim_rate)

  if (is.na(probability) || length(probability) != 1) {
    stop("probability doit être une probabilité numérique unique.")
  }

  if (risk_segment %in% c("Très élevé") || lift >= 8) {
    return(
      paste(
        "Revue underwriting prioritaire : score très supérieur au portefeuille,",
        "analyse détaillée du dossier recommandée avant toute décision."
      )
    )
  }

  if (risk_segment %in% c("Élevé") || lift >= 5) {
    return(
      paste(
        "Analyse complémentaire recommandée : vérifier les caractéristiques du voyage,",
        "le produit, la destination et les règles internes de souscription."
      )
    )
  }

  if (risk_segment %in% c("Modéré à élevé", "Modéré") || lift >= 2) {
    return(
      paste(
        "Revue standard conseillée : le dossier présente un score supérieur à la moyenne,",
        "mais doit être interprété avec les autres critères métier."
      )
    )
  }

  paste(
    "Aucune alerte particulière selon le score : traitement standard possible,",
    "sous réserve des règles internes et des informations complémentaires du dossier."
  )
}
