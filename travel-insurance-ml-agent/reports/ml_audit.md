# Audit global de la logique Machine Learning

## Objet de l'audit

Cet audit vérifie la cohérence du pipeline avec l'objectif réel du mémoire : prédire la survenance d'un sinistre (`claim_status = yes`) en assurance voyage.

## Points corrigés

- La cible est `claim_status` et non l'achat d'une assurance.
- La classe positive est explicitement `yes`.
- SMOTE est appliqué uniquement dans les folds de validation croisée ou sur le train lors du fit final.
- Le test set reste non modifié.
- Les métriques de classification sont calculées sur la classe positive `yes`.
- La comparaison des modèles utilise les probabilités out-of-fold et un seuil calibré par configuration.
- Le seuil final du champion est repris depuis la validation croisée, et non choisi sur le test set.

## Lecture des résultats

Le problème est fortement déséquilibré : les sinistres représentent une très faible part des observations. L'accuracy est donc peu informative seule. La ROC AUC mesure la capacité de discrimination, tandis que le recall et le F1-score indiquent la capacité à détecter effectivement les sinistres après choix d'un seuil.

## Modèle champion

Le champion retenu est sélectionné selon :

1. ROC AUC ;
2. F1-score ;
3. recall sur la classe `yes`.

Avec les résultats obtenus, le champion est `xgboost__none`.

## Limites restantes

- CatBoost n'est pas exécuté car le package R n'est pas disponible via CRAN pour l'environnement utilisé.
- Les hyperparamètres sont volontairement simples pour garantir un pipeline reproductible.
- Le seuil est calibré selon le F1-score ; en production actuarielle, il devrait être ajusté selon un coût métier des faux positifs et faux négatifs.
- Les résultats doivent être interprétés comme une preuve de concept académique, non comme un outil de souscription ou de gestion de sinistre prêt pour production.
