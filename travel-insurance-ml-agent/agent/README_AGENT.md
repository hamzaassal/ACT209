# Agent IA explicatif underwriting

## Rôle

L'agent IA transforme une sortie technique du modèle XGBoost en explication métier utilisable par un souscripteur assurance voyage.

Il explique :

- la probabilité estimée de sinistre ;
- le segment de risque ;
- le lift par rapport au portefeuille ;
- la recommandation underwriting ;
- les limites du modèle.

Il ne prend jamais de décision automatique.

## Fichiers

- `agent_prompt.md` : prompt système de l'assistant underwriting.
- `build_agent_context.R` : construit le contexte métier envoyé à l'agent.
- `explain_prediction.R` : génère une explication déterministe locale.
- `generate_recommendation.R` : produit une recommandation underwriting déterministe.
- `call_llm_agent.R` : appelle un LLM si une clé API est disponible, sinon utilise le fallback local.

## Mode fallback

Sans variable d'environnement `OPENAI_API_KEY`, l'agent reste fonctionnel. Il combine :

- l'explication locale du score ;
- la recommandation déterministe ;
- les limites du modèle.

Ce mode permet de tester l'application sans dépendance externe.

## Mode LLM

Pour activer l'appel externe :

```r
Sys.setenv(OPENAI_API_KEY = "votre-cle-api")
Sys.setenv(OPENAI_MODEL = "gpt-4o-mini")
```

Aucune clé n'est codée en dur dans le projet.
