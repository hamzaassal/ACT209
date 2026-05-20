# Application Shiny - Travel Insurance Risk Scoring

## Objectif

Cette application est un prototype professionnel d'aide à la décision pour un souscripteur assurance voyage. Elle utilise le modèle champion XGBoost sans SMOTE pour produire un score de risque de sinistre (`claim_status = yes`).

Le score doit être interprété comme un outil de priorisation et d'analyse, pas comme une décision automatique.

## Prérequis

- R récent installé.
- Les packages R nécessaires : `shiny`, `bslib`, `yaml`, `dplyr`, `tidymodels`, `httr`, `jsonlite`.
- Les fichiers suivants doivent exister :
  - `models/best_model.rds`
  - `models/model_metadata.rds`
  - `config/risk_segments.yml`
  - `config/app_config.yml`

## Lancement

Depuis la racine du projet :

```r
shiny::runApp("shiny_app")
```

## Fonctionnement

1. Ouvrir l'onglet **Saisie dossier**.
2. Renseigner les caractéristiques du dossier.
3. Cliquer sur **Calculer le score de risque**.
4. Lire le score, le segment, le lift et la recommandation.
5. Interroger l'agent IA dans l'onglet **Agent IA** si une explication métier est souhaitée.

## Lecture du score

Le score correspond à une probabilité estimée de sinistre. Les segments de risque sont dérivés de l'analyse de lift sur le test set :

- Top 1 % : segment très élevé.
- Top 5 % : segment élevé.
- Top 10 % : segment modéré à élevé.
- Top 20 % : segment modéré.
- Hors Top 20 % : segment standard.

## Agent IA

L'agent IA explique le score et propose une recommandation underwriting. Il fonctionne en deux modes :

- **Mode LLM** si la variable d'environnement `OPENAI_API_KEY` est définie.
- **Mode fallback local** si aucune clé n'est disponible.

Pour activer le mode LLM :

```r
Sys.setenv(OPENAI_API_KEY = "votre-cle-api")
```

Optionnellement :

```r
Sys.setenv(OPENAI_MODEL = "gpt-4o-mini")
```

## Limites connues

- Le modèle ne prédit pas une certitude de sinistre.
- Les segments utilisent des seuils de score issus du test set, documentés dans `config/risk_segments.yml`.
- L'application ne remplace pas les règles internes de souscription.
- L'interprétation fine par facteurs explicatifs sera renforcée lorsque des valeurs SHAP seront disponibles.

## Erreurs fréquentes

- `models/best_model.rds introuvable` : relancer le pipeline ML ou vérifier le dossier `models/`.
- Package manquant : installer le package indiqué par le message d'erreur.
- Réponse agent fallback : aucune clé API LLM n'est configurée, ce qui est normal en mode local.
