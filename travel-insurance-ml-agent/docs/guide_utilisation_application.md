# Guide utilisateur de l'application

## 1. Objectif

L'application aide un underwriter a analyser un dossier d'assurance voyage. Elle fournit un score de risque de sinistre, un segment de risque, un lift et une recommandation metier.

Elle ne remplace pas l'expertise humaine et ne produit pas de decision contractuelle automatique.

## 2. Public cible

Le prototype s'adresse a un souscripteur, un charge d'affaires assurance, un actuaire ou un analyste portefeuille.

## 3. Lancer l'application

Depuis la racine du projet :

```r
shiny::runApp("shiny_app")
```

## 4. Utiliser l'application

1. Ouvrir **Scoring d'un dossier**.
2. Renseigner les caracteristiques disponibles avant tarification.
3. Cliquer sur **Calculer le score de risque**.
4. Ouvrir **Analyse du risque** pour lire le score, le segment, le lift et la recommandation.
5. Ouvrir **Agent IA explicatif** pour obtenir une synthese metier.

## 5. Lire les indicateurs

- **Score de risque estime** : probabilite estimee de sinistre.
- **Segment de risque** : position du dossier parmi les scores les plus eleves.
- **Lift** : niveau de concentration du risque par rapport au taux moyen.
- **Recommandation underwriting** : conseil d'analyse, jamais decision automatique.

## 6. Couleurs

- Vert : aucune alerte particuliere selon le modele.
- Bleu ou jaune : revue standard ou vigilance metier.
- Orange : analyse complementaire conseillee.
- Rouge : analyse underwriting prioritaire.

## 7. Exemple

Si un dossier est classe dans le Top 10 %, cela signifie qu'il appartient aux 10 % de dossiers ayant les scores de risque les plus eleves. Dans l'echantillon de test du modele complet, ce segment concentrait 53,76 % des sinistres observes.

## 8. Agent IA

L'agent peut repondre a des questions comme :

- Pourquoi ce dossier est-il considere risque ?
- Quelle recommandation underwriting proposer ?
- Que signifie le lift affiche ?
- Quelles sont les limites du modele ?
- Comment interpreter ce score face au taux moyen du portefeuille ?

Sans cle API, l'agent fonctionne en mode local et genere une reponse deterministe.

## 9. Bonnes pratiques

- Utiliser le score pour prioriser, pas pour decider automatiquement.
- Croiser le score avec les regles internes de souscription.
- Documenter toute decision sensible.
- Recalibrer les seuils si le portefeuille evolue.
- Ne jamais presenter le score comme une certitude individuelle.
