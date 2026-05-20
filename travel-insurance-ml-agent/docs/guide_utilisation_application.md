# Guide d'utilisation de l'application

## 1. Objectif de l'application

L'application estime un score de risque de sinistre pour un dossier d'assurance voyage. Elle aide le souscripteur à prioriser les dossiers les plus sensibles, sans produire de décision automatique.

## 2. Public cible

Le prototype est destiné à un underwriter, un chargé d'affaires assurance, un analyste portefeuille ou un responsable actuariat souhaitant comprendre le risque relatif d'un dossier.

## 3. Lancement de l'application

Depuis la racine du projet R :

```r
shiny::runApp("shiny_app")
```

## 4. Saisie d'un dossier

L'onglet **Saisie dossier** permet de renseigner les variables attendues par le modèle :

- agence ;
- type d'agence ;
- canal de distribution ;
- produit ;
- durée ;
- destination ;
- ventes nettes ;
- commission ;
- genre ;
- âge.

Les valeurs par défaut servent à tester l'application et ne constituent pas une recommandation métier.

## 5. Lecture du score de risque

Le score affiché correspond à une probabilité estimée de sinistre. Il doit être lu comme un indicateur de risque relatif, pas comme une certitude.

Exemple : un score de 8 % ne signifie pas que le sinistre aura lieu. Il signifie que le dossier présente un niveau de risque supérieur à la fréquence moyenne observée dans le portefeuille test, qui est d'environ 1,68 %.

## 6. Comprendre le segment de risque

Les segments sont issus de l'analyse de concentration du risque :

- Top 1 % : dossiers les plus risqués.
- Top 5 % : segment de ciblage prioritaire.
- Top 10 % : segment important pour la surveillance.
- Top 20 % : segment large de priorisation.
- Standard : dossier hors zones de score élevé.

## 7. Comprendre le lift

Le lift mesure combien le taux de sinistre du segment est supérieur au taux moyen du test set.

Formule :

```text
Lift = taux de sinistre du segment / taux moyen de sinistre du test set
```

Un lift de 5 signifie que le segment est environ cinq fois plus sinistré que la moyenne.

## 8. Utiliser l'agent IA

L'onglet **Agent IA** permet de poser une question en langage naturel, par exemple :

- Pourquoi ce dossier est-il risqué ?
- Quelle recommandation underwriting ?
- Que signifie le lift ?
- Quelles sont les limites du modèle ?

L'agent reçoit automatiquement le score, le segment, le lift, la recommandation et les caractéristiques du dossier.

## 9. Exemples d'interprétation

### Dossier à faible risque

Un dossier avec un score inférieur aux seuils Top 20 % sera classé **Standard**. La recommandation sera généralement de ne pas déclencher d'alerte particulière selon le score, tout en appliquant les règles internes habituelles.

### Dossier à risque élevé

Un dossier dans le Top 5 % présente un taux de sinistre historique de 11,03 % dans le segment, contre 1,68 % en moyenne. L'application recommande une analyse complémentaire avant décision.

### Question posée à l'agent

Question : "Pourquoi ce dossier mérite-t-il une revue ?"

Réponse attendue : l'agent explique le score, le compare au taux moyen, décrit le segment de risque et rappelle qu'il s'agit d'une aide à l'analyse.

## 10. Limites et précautions

- Le score ne prouve pas qu'un sinistre aura lieu.
- Le modèle peut générer des faux positifs.
- Les résultats dépendent de la qualité des données historiques.
- Les règles contractuelles, juridiques et commerciales restent prioritaires.

## 11. Bonnes pratiques underwriting

- Utiliser le score pour prioriser les dossiers, pas pour automatiser une décision.
- Comparer le score aux informations qualitatives disponibles.
- Documenter les décisions sensibles.
- Réviser régulièrement les seuils si le portefeuille évolue.
