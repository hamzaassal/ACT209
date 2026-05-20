# Procédure de création de l'agent IA explicatif

## 1. Objectif de l'agent IA

L'agent IA transforme une sortie technique du modèle de machine learning en explication métier. Il aide l'underwriter à comprendre le score de risque de sinistre, le segment associé et les limites de l'analyse.

L'agent n'est pas un décideur automatique. Il contextualise, explique et recommande une action d'analyse.

## 2. Architecture générale

Flux fonctionnel :

```text
Données client saisies dans Shiny
-> modèle XGBoost
-> probabilité de sinistre
-> segmentation du risque
-> construction du contexte agent
-> prompt système
-> appel LLM ou fallback local
-> réponse métier affichée dans Shiny
```

## 3. Composants de l'agent

- `agent_prompt.md` définit le rôle, le ton, les limites et la structure de réponse.
- `build_agent_context.R` transforme les données client et le résultat de scoring en texte structuré.
- `explain_prediction.R` génère une explication locale déterministe.
- `generate_recommendation.R` produit une recommandation underwriting sans dépendance externe.
- `call_llm_agent.R` orchestre l'appel LLM ou le fallback local.
- `mod_agent_chat.R` intègre l'agent dans l'interface Shiny.

## 4. Construction du prompt système

Le prompt impose à l'agent :

- de répondre en français clair ;
- de se positionner comme assistant underwriting assurance voyage ;
- d'expliquer une probabilité de sinistre, pas une certitude ;
- de comparer le score au taux moyen du portefeuille ;
- de structurer la réponse en synthèse, explication, recommandation, vigilance et limites ;
- de ne jamais prendre de décision automatique.

## 5. Construction du contexte envoyé à l'agent

Le contexte contient :

- les variables du dossier ;
- la probabilité estimée de sinistre ;
- le seuil opérationnel utilisé ;
- la classe prédite au seuil ;
- le segment de risque ;
- le taux de sinistre historique du segment ;
- le lift ;
- le taux moyen de sinistre ;
- la recommandation déterministe ;
- les limites du modèle.

## 6. Interaction avec le modèle

Le modèle XGBoost calcule uniquement une probabilité. L'agent ne modifie pas cette probabilité, ne réentraîne pas le modèle et ne change pas les seuils.

Son rôle est d'interpréter la sortie du modèle pour un utilisateur métier.

## 7. Interaction avec Shiny

Dans Shiny :

1. L'utilisateur saisit un dossier.
2. `predict_claim_risk()` appelle le modèle.
3. Shiny affiche le score et le segment.
4. L'utilisateur pose une question.
5. `call_llm_agent()` reçoit le contexte et renvoie une réponse.

## 8. Gestion du mode fallback

Si `OPENAI_API_KEY` n'est pas définie, l'application continue à fonctionner. Elle utilise :

- `explain_prediction()` pour expliquer le score ;
- `generate_recommendation()` pour produire une recommandation ;
- le contexte construit localement.

Ce choix garantit la robustesse du prototype et facilite la démonstration hors connexion.

## 9. Sécurité et limites

L'agent ne doit pas :

- annoncer qu'un sinistre est certain ;
- décider l'acceptation ou le refus d'un contrat ;
- produire un avis juridique définitif ;
- inventer des variables absentes ;
- remplacer l'underwriter.

Une validation humaine reste indispensable.

## 10. Évolutions possibles

- Ajouter des valeurs SHAP pour expliquer les facteurs individuels.
- Historiser les questions et réponses.
- Générer un export PDF de l'analyse.
- Comparer plusieurs scénarios de souscription.
- Connecter une API LLM en production.
- Ajouter des règles métier internes.
- Surveiller le drift de données et de performance.
