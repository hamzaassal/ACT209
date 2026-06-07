# Architecture modÃ¨le - Shiny - agent IA

## SchÃ©ma textuel

```text
Utilisateur mÃ©tier
  -> Application Shiny
  -> Module de saisie dossier
  -> Fonction predict_claim_risk()
  -> ModÃ¨le XGBoost champion
  -> ProbabilitÃ© de sinistre
  -> Segmentation du risque
  -> Affichage score / lift / recommandation
  -> Construction contexte agent
  -> Agent LLM ou fallback local
  -> Explication underwriting
```

## RÃ´le de chaque composant

### ModÃ¨le ML

Le modÃ¨le `models/best_model.rds` estime la probabilitÃ© de sinistre. Il ne prend pas de dÃ©cision contractuelle.

### Application Shiny

Shiny fournit une interface mÃ©tier pour saisir un dossier, afficher le score, le segment et la recommandation.

### Configuration

Les fichiers `config/risk_segments.yml` et `config/app_config.yml` centralisent les seuils, les taux historiques, le lift et les paramÃ¨tres applicatifs.

### Agent IA

L'agent explique le score et contextualise le rÃ©sultat pour un souscripteur. Il peut utiliser un LLM externe ou fonctionner en mode fallback local.

## Flux de donnÃ©es

Les donnÃ©es saisies dans l'application sont transformÃ©es en `data.frame` d'une ligne. Cette ligne est envoyÃ©e au workflow tidymodels sauvegardÃ©. La probabilitÃ© `.pred_yes` est rÃ©cupÃ©rÃ©e, comparÃ©e au seuil opÃ©rationnel et utilisÃ©e pour attribuer un segment de risque.

Le contexte agent est ensuite construit Ã  partir des mÃªmes donnÃ©es et du rÃ©sultat de scoring.

## SÃ©paration des responsabilitÃ©s

- `shiny_app/modules/mod_input_client.R` : collecte des donnÃ©es.
- `shiny_app/global.R` : chargement du modÃ¨le, configuration et fonction de scoring.
- `shiny_app/modules/mod_score_output.R` : restitution synthÃ©tique.
- `shiny_app/modules/mod_agent_chat.R` : interface conversationnelle, contexte de scoring et explication locale.
- `agent/*.R` : logique explicative et appel LLM.

## Limites

- Les seuils de segment sont issus d'une analyse sur le test set et doivent Ãªtre recalibrÃ©s en production.
- Les explications locales SHAP sont utilisÃ©es lorsque le workflow XGBoost les expose correctement ; sinon l'agent affiche un message de fallback.
- L'agent ne remplace pas la validation underwriting humaine.

