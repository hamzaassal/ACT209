# Architecture modèle - Shiny - agent IA

## Schéma textuel

```text
Utilisateur métier
  -> Application Shiny
  -> Module de saisie dossier
  -> Fonction predict_claim_risk()
  -> Modèle XGBoost champion
  -> Probabilité de sinistre
  -> Segmentation du risque
  -> Affichage score / lift / recommandation
  -> Construction contexte agent
  -> Agent LLM ou fallback local
  -> Explication underwriting
```

## Rôle de chaque composant

### Modèle ML

Le modèle `models/best_model.rds` estime la probabilité de sinistre. Il ne prend pas de décision contractuelle.

### Application Shiny

Shiny fournit une interface métier pour saisir un dossier, afficher le score, le segment et la recommandation.

### Configuration

Les fichiers `config/risk_segments.yml` et `config/app_config.yml` centralisent les seuils, les taux historiques, le lift et les paramètres applicatifs.

### Agent IA

L'agent explique le score et contextualise le résultat pour un souscripteur. Il peut utiliser un LLM externe ou fonctionner en mode fallback local.

## Flux de données

Les données saisies dans l'application sont transformées en `data.frame` d'une ligne. Cette ligne est envoyée au workflow tidymodels sauvegardé. La probabilité `.pred_yes` est récupérée, comparée au seuil opérationnel et utilisée pour attribuer un segment de risque.

Le contexte agent est ensuite construit à partir des mêmes données et du résultat de scoring.

## Séparation des responsabilités

- `shiny_app/modules/mod_input_client.R` : collecte des données.
- `shiny_app/global.R` : chargement du modèle, configuration et fonction de scoring.
- `shiny_app/modules/mod_score_output.R` : restitution synthétique.
- `shiny_app/modules/mod_risk_explanation.R` : analyse métier déterministe.
- `shiny_app/modules/mod_agent_chat.R` : interface conversationnelle.
- `agent/*.R` : logique explicative et appel LLM.

## Limites

- Les seuils de segment sont issus d'une analyse sur le test set et doivent être recalibrés en production.
- Les explications locales détaillées nécessitent l'ajout futur de SHAP values.
- L'agent ne remplace pas la validation underwriting humaine.
