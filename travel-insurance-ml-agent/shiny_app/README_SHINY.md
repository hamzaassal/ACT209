# Application Shiny - Risk Scoring & Agent IA

## Objectif

Cette application est un prototype professionnel d'aide a l'underwriting en assurance voyage. Elle estime un score de risque de sinistre, positionne le dossier dans un segment de risque et propose une explication metier via un agent IA.

L'application charge en priorite le modele applicatif sans variables tarifaires :

- `models/no_tariff_best_model.rds`
- `models/no_tariff_model_metadata.rds`

Ce choix evite de fonder la recommandation sur une prime ou une commission deja calculee. Si ces fichiers sont absents, l'application utilise le modele complet historique comme fallback.

## Lancement

Depuis la racine du projet :

```r
shiny::runApp("shiny_app")
```

## Pages

- **Accueil** : contexte, positionnement metier et parcours d'utilisation.
- **Scoring d'un dossier** : saisie des caracteristiques disponibles avant tarification.
- **Analyse du risque** : score, segment, lift, taux du segment et recommandation underwriting.
- **Agent IA explicatif** : questions libres ou predefinies sur le dossier score.
- **Methodologie & limites** : cible, approche scoring/ranking, desequilibre de classes et limites.
- **Guide utilisateur** : lecture des indicateurs, couleurs et bonnes pratiques.

## Fichiers necessaires

- `models/no_tariff_best_model.rds`
- `models/no_tariff_model_metadata.rds`
- `config/risk_segments.yml`
- `config/app_config.yml`
- `shiny_app/www/custom.css`
- `shiny_app/www/logo-cnam.png` si le logo est disponible

## Agent IA

L'agent recoit automatiquement le dossier, le score, le seuil, le segment, le lift, la recommandation et les limites du modele. Il ne prend jamais de decision automatique.

Sans cle API, l'application fonctionne en mode local :

```text
Mode local : reponse generee sans appel externe a un LLM.
```

Pour activer un LLM externe :

```r
Sys.setenv(OPENAI_API_KEY = "votre-cle-api")
Sys.setenv(OPENAI_MODEL = "gpt-4o-mini")
```

## Test

```r
source("scripts/08_test_shiny_agent_components.R")
```

Le script teste le chargement du modele, le logo, le CSS, la prediction, la segmentation, la recommandation, les contributions locales et le fallback agent.

## Limites

- Le score est probabiliste.
- Le modele sert a prioriser l'analyse, pas a automatiser une decision.
- Les seuils doivent etre recalibres si le modele change.
- Toute utilisation operationnelle necessite une validation metier et un suivi du drift.
