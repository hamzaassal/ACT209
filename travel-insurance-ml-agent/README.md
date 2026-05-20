# travel-insurance-ml-agent

Projet R/Shiny de Master 2 Actuariat sur l'application du Machine Learning et des agents IA à l'assurance voyage.

L'objectif n'est pas de prédire l'achat d'une assurance. Le projet prédit la **survenance d'un sinistre** à partir du dataset Kaggle Travel Insurance.

## Cible

La variable cible est standardisée sous le nom :

```text
claim_status
```

Modalités :

- `no` : absence de sinistre ;
- `yes` : sinistre observé, classe positive.

Le fichier local contient une colonne `Claim`, renommée automatiquement en `claim_status` après nettoyage des noms de colonnes.

## Structure

```text
travel-insurance-ml-agent/
├── data/
│   ├── raw/              # CSV Kaggle ajouté manuellement
│   ├── processed/        # Données nettoyées, train/test, recette
│   └── external/
├── scripts/              # Pipeline R
├── shiny_app/            # Application Shiny
├── agent/                # Prompt et fonctions d'explication
├── reports/              # Rapport, métriques, graphiques
├── models/               # Modèles entraînés et champion
├── README.md
└── renv.lock
```

## Ordre d'exécution

Depuis la racine du projet :

```r
source("scripts/01_import_data.R")
source("scripts/02_eda.R")
source("scripts/03_preprocessing.R")
source("scripts/04_model_training.R")
source("scripts/05_model_evaluation.R")
source("scripts/06_save_model.R")
source("scripts/07_bootstrap_validation.R")
source("scripts/08_threshold_analysis.R")
source("scripts/09_hyperparameter_tuning.R")
source("scripts/09_smote_training_audit.R")
source("scripts/10_risk_ranking_lift_analysis.R")
source("scripts/11_smote_intensity_analysis.R")
```

## Pipeline

- `01_import_data.R` : lit le CSV dans `data/raw/`, nettoie les noms, identifie `claim_status`, vérifie les doublons et valeurs manquantes, sauvegarde `data_clean.rds`.
- `02_eda.R` : produit les statistiques descriptives, l'analyse de la cible, le déséquilibre de classes et les graphiques.
- `03_preprocessing.R` : crée le split train/test 80/20 stratifié et la recette `tidymodels`.
- `04_model_training.R` : compare les modèles en validation croisée stratifiée 5-fold.
- `05_model_evaluation.R` : évalue le champion sur le test set non modifié avec un seuil calibré hors test.
- `06_save_model.R` : sauvegarde `models/best_model.rds` et `models/model_metadata.rds`.
- `07_bootstrap_validation.R` : mesure la stabilité du modèle champion par bootstrap.
- `08_threshold_analysis.R` : analyse le compromis precision/recall selon les seuils du modèle champion.
- `09_hyperparameter_tuning.R` : teste un tuning raisonnable de XGBoost, XGBoost pondéré, XGBoost avec SMOTE léger et Random Forest.
- `09_smote_training_audit.R` : vérifie que SMOTE est appliqué uniquement au train/folds.
- `10_risk_ranking_lift_analysis.R` : analyse le pouvoir de scoring et le lift du champion XGBoost sans SMOTE.
- `11_smote_intensity_analysis.R` : compare `none`, `smote_10` et `smote_20` pour tester l'intensité du rééquilibrage.

## Modèles comparés

Stratégies :

- `none` : sans rééquilibrage ;
- `smote` : SMOTE appliqué uniquement au train ou à l'intérieur des folds ;
- analyse complémentaire : `smote_10` et `smote_20` pour tester un rééquilibrage plus modéré.

Modèles entraînés :

- régression logistique ;
- arbre de décision ;
- random forest ;
- XGBoost.

CatBoost est conservé dans la structure du benchmark. Dans l'environnement R utilisé, le package `catboost` n'est pas disponible via CRAN ; les configurations CatBoost sont donc tracées comme `skipped` dans `reports/model_comparison.csv`.

## Métriques

Les métriques calculées sont :

- accuracy ;
- precision sur `yes` ;
- recall sur `yes` ;
- F1-score sur `yes` ;
- ROC AUC ;
- PR AUC.

L'accuracy n'est pas la métrique principale, car les sinistres sont rares. Le modèle champion est sélectionné selon :

1. ROC AUC ;
2. F1-score ;
3. recall sur la classe `yes`.

## Fichiers produits

Principaux artefacts :

- `data/processed/data_clean.rds`
- `data/processed/train_data.rds`
- `data/processed/test_data.rds`
- `data/processed/recipe_base.rds`
- `reports/model_comparison.csv`
- `reports/model_thresholds.csv`
- `reports/class_balance_summary.csv`
- `reports/training_data_audit.csv`
- `reports/classification_metrics_by_model.csv`
- `reports/smote_effect_on_metrics.csv`
- `reports/model_comparison_smote_intensity.csv`
- `reports/smote_intensity_class_balance.csv`
- `reports/xgboost_lift_by_smote_intensity.csv`
- `reports/hyperparameter_tuning_results.csv`
- `reports/best_tuned_models.csv`
- `reports/tuned_model_test_metrics.csv`
- `reports/tuned_model_lift_analysis.csv`
- `reports/champion_vs_tuned_comparison.csv`
- `reports/final_metrics.csv`
- `reports/bootstrap_results.csv`
- `reports/bootstrap_summary.csv`
- `reports/threshold_analysis.csv`
- `reports/selected_thresholds.csv`
- `reports/risk_ranking_lift_table.csv`
- `models/best_model.rds`
- `models/model_metadata.rds`

Les graphiques sont sauvegardés dans :

```text
reports/figures/
```

## Rapport

Le rapport principal est :

```text
reports/projet_memoire.Rmd
```

Il charge les vrais fichiers produits par le pipeline, affiche les métriques et graphiques disponibles, et interprète les résultats dans une logique actuarielle et métier.

## Agent IA

Le futur agent IA explicatif devra :

- expliquer la probabilité de sinistre ;
- interpréter les facteurs de risque ;
- formuler une recommandation métier ;
- utiliser un langage clair pour un chargé d'affaires assurance ou un actuaire.

L'agent doit présenter le score comme une estimation probabiliste, jamais comme une certitude de sinistre.
