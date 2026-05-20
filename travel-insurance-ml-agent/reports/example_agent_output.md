# Exemple de sortie agent

## Dossier fictif

  agency agency_type distribution_channel      product_name duration
1    C2B    Airlines               Online Cancellation Plan       15
  destination net_sales commision_in_value gender age
1   SINGAPORE        80                 20      F  35

## Résultat scoring

- Probabilité de sinistre : 5.09 %
- Classe au seuil : yes
- Seuil utilisé : 5 %
- Segment : Modéré à élevé (Top 10 %)
- Taux de sinistre segment : 9.04 %
- Lift : 5.37

## Contexte transmis à l'agent

```text
CONTEXTE DOSSIER ASSURANCE VOYAGE

Caractéristiques du dossier :
- agency : C2B
- agency_type : Airlines
- distribution_channel : Online
- product_name : Cancellation Plan
- duration : 15
- destination : SINGAPORE
- net_sales : 80
- commision_in_value : 20
- gender : F
- age : 35

Résultat du modèle :
- Probabilité estimée de sinistre : 5.09 %
- Seuil opérationnel utilisé : 5 %
- Classe prédite au seuil : yes
- Segment de risque : Modéré à élevé (Top 10 %)
- Taux de sinistre historique du segment : 9.04 %
- Lift du segment : 5.37
- Taux moyen de sinistre du portefeuille : 1.68 %
- Recommandation automatique : Analyse complémentaire recommandée : vérifier les caractéristiques du voyage, le produit, la destination et les règles internes de souscription.

Limites à rappeler :
- Le score est probabiliste.
- Le modèle ne prouve pas qu'un sinistre aura lieu.
- Le modèle ne remplace pas l'analyse de l'underwriter.
- Les règles internes de souscription restent prioritaires.

```

## Réponse agent

Mode fallback : aucune clé API LLM détectée, réponse générée localement.

Question : Pourquoi ce dossier doit-il être analysé ?

1. Synthèse du risque
Le modèle estime une probabilité de sinistre de 5.09 %. Le taux moyen de sinistre du portefeuille de référence est d'environ 1.68 %. Le dossier est classé dans le segment de risque 'Modéré à élevé', associé à un taux historique de sinistre de 9.04 % et à un lift de 5.37. Cela signifie que ce segment est plus sinistré que la moyenne du portefeuille. Cette sortie doit être utilisée comme un score de priorisation, pas comme une décision automatique.

L'interprétation fine variable par variable pourra être enrichie ultérieurement avec des valeurs SHAP. Aucune explication SHAP locale n'est actuellement disponible dans les métadonnées du modèle.

2. Recommandation underwriting
Analyse complémentaire recommandée : vérifier les caractéristiques du voyage, le produit, la destination et les règles internes de souscription.

3. Points de vigilance
Ce score est une aide à la décision. Il doit être complété par l'analyse humaine, les règles internes et les informations contractuelles disponibles.
