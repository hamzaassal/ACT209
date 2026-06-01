# Exemple de sortie agent

## Contrôles techniques

                              check status
1                      model_loaded   TRUE
2                   metadata_loaded   TRUE
3                    logo_in_assets   TRUE
4                       logo_in_www   TRUE
5                 custom_css_exists   TRUE
6  prediction_probability_available   TRUE
7            risk_segment_available   TRUE
8              local_shap_available   TRUE
9           global_shap_file_exists   TRUE
10          agent_context_available   TRUE
11        fallback_answer_available   TRUE

## Dossier fictif

  agency agency_type distribution_channel      product_name duration
1    C2B    Airlines               Online Cancellation Plan       15
  destination gender age
1   SINGAPORE      F  35

## Résultat scoring

- Probabilité de sinistre : 4.15 %
- Classe au seuil : no
- Seuil utilisé : 9.00 %
- Segment : Modéré (Top 20 %)
- Taux de sinistre segment : 5.92 %
- Lift : 3.52
- Recommandation : Revue standard conseillée : le dossier présente un score supérieur à la moyenne, mais doit être interprété avec les autres critères métier.

## Contributions SHAP locales

# A tibble: 6 × 4
  feature_group         shap_value abs_shap direction        
  <chr>                      <dbl>    <dbl> <chr>            
1 Agence                   -2.30     2.44   Diminue le score 
2 Produit                   0.432    0.651  Augmente le score
3 Destination               0.259    0.419  Augmente le score
4 Canal de distribution    -0.179    0.179  Diminue le score 
5 Durée du voyage           0.138    0.138  Augmente le score
6 Âge                      -0.0381   0.0381 Diminue le score 

## Contexte transmis à l'agent

```text
CONTEXTE DOSSIER ASSURANCE VOYAGE

Caracteristiques du dossier :
- agency : C2B
- agency_type : Airlines
- distribution_channel : Online
- product_name : Cancellation Plan
- duration : 15
- destination : SINGAPORE
- gender : F
- age : 35

Resultat du modele :
- Modele utilise : XGBoost sans SMOTE - approche underwriting sans variable tarifaire (seuil 9.00 %)
- Positionnement methodologique : Modèle applicatif sans variable tarifaire
- Variables tarifaires exclues : net_sales, commision_in_value
- Probabilite estimee de sinistre : 4.15 %
- Seuil operationnel utilise : 9 %
- Classe predite au seuil : no
- Segment de risque : Modéré (Top 20 %)
- Taux de sinistre historique du segment : 5.92 %
- Lift du segment : 3.52
- Taux moyen de sinistre du portefeuille : 1.68 %
- Recommandation automatique : Revue standard conseillée : le dossier présente un score supérieur à la moyenne, mais doit être interprété avec les autres critères métier.

Limites a rappeler :
- Le score est probabiliste.
- Le modele ne prouve pas qu'un sinistre aura lieu.
- Le modele ne remplace pas l'analyse de l'underwriter.
- La recommandation applicative ne repose pas sur une prime deja calculee.
- Les regles internes de souscription restent prioritaires.

```

## Réponse agent

Mode fallback : l'appel LLM a echoue (Could not connect to server [api.openai.com]:
Failed to connect to api.openai.com port 443 after 17 ms: Could not connect to server).

Reponse locale de secours generee a partir du score, du segment et des regles metier du projet.

Question : Pourquoi ce dossier doit-il être analysé ?

1. Synthese du risque
Le dossier presente un score de sinistre estime a 4.15 %. Il est classe dans le segment Modéré (Top 20 %), avec un lift de 3.52 par rapport au taux moyen du portefeuille.

2. Elements explicatifs
Le modèle estime une probabilité de sinistre de 4.15 %. Le taux moyen de sinistre du portefeuille de référence est d'environ 1.68 %. Le dossier est classé dans le segment de risque 'Modéré', associé à un taux historique de sinistre de 5.92 % et à un lift de 3.52. Cela signifie que ce segment est plus sinistré que la moyenne du portefeuille. Cette sortie doit être utilisée comme un score de priorisation, pas comme une décision automatique.

Principales contributions SHAP locales pour ce dossier : Agence (Diminue le score), Produit (Augmente le score), Destination (Augmente le score), Canal de distribution (Diminue le score), Durée du voyage (Augmente le score), Âge (Diminue le score). Ces contributions indiquent les groupes de variables qui influencent le plus le score du dossier, à la hausse ou à la baisse.

3. Recommandation underwriting
Revue standard conseillée : le dossier présente un score supérieur à la moyenne, mais doit être interprété avec les autres critères métier.

4. Points de vigilance
Le score doit etre compare aux informations du dossier et aux regles internes de souscription. Un score eleve indique une priorite d'analyse, pas une certitude de sinistre.

5. Limites du modele
Le modele est entraine sur une base historique et ne prouve pas qu'un sinistre aura lieu. Il ne remplace pas l'underwriter, ne constitue pas une decision juridique ou contractuelle, et doit etre utilise comme outil d'aide a la priorisation.
