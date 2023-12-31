# Analyse Géostatistique : Spotify vs FMA

## Source des données utilisées : 
- **Données Spotify** : https://www.kaggle.com/datasets/asaniczka/top-spotify-songs-in-73-countries-daily-updated (Version ..)
- **Données FMA** : https://github.com/mdeff/fma
- **Code ISO** : https://sql.sh/514-liste-pays-csv-xml#google_vignette
- **Données IDH** : https://view.officeapps.live.com/op/view.aspx?src=https%3A%2F%2Fhdr.undp.org%2Fsites%2Fdefault%2Ffiles%2F2021-22_HDR%2FHDR21-22_Statistical_Annex_HDI_Table.xlsx&wdOrigin=BROWSELINK
- **Données pays du monde format SHP** : https://public.opendatasoft.com/explore/dataset/world-administrative-boundaries/table/
- **Dossier github "documentation"** : Regroupe des documents en complément intéressants pour notre étude
- **Dossier github "image_resultats"** : Regroupe toutes les images utilisées dans le rapport

## Objectifs de l'analyse : 
L'objectif de cette étude est de comparer deux jeux de données : un venant de l'API Spotify (téléchargé sur le site *Kaggle*) avec le Top 50 des chansons de 70 pays différent (dont le classement global) et un autre venant du site *https://freemusicarchive.org/home* (téléchargé sur le répertoire github de Mr Michaël Defferrard), une plateforme d'hébergement de musiques amateurs. 
Cette comparaison reposant sur la problématique suivante : **Quels sont les facteurs contribuant à la popularité d’une musique considérée comme un hit comparés à une chanson amateur selon différentes échelles spatiales ?**

## Méthodologies et résultats de l'analyse :
Pour consulter la méthodologie et les résultats de cette analyse, le rapport *Rapport Projet Géostat ING3 Spotify_FMA* et la présentation *Présentation projet Géostatistique Spotify vs FMA* sont disponibles dans le dossier "Rapport & Présentation" du répertoire github ainsi que les codes R.

## Les différents dossiers du répertoire
- "Codes R" : Dossier dans lequel sont répertoriés tous les scripts en language R de l'étude
- "Final_Data" : Dossier où se trouve les données finales utilisées pour les méthodes d'analyses. Le choix de garder ce dossier s'est décidé afin d'éviter de devoir refaire "runner" tout le code et pouvoir les utiliser directement
- "Rapport & Présentation" : voir section précédente
- "documentation" : Dossier où il est possible de retrouver un article scientifique qui a servis pour la définition des différents attributs utilisés dans les analyses ainsi que le dictionnaires des métadonnées des attributs.
- "image_resultats" : Dossier regroupant toutes les images utilisées dans le rapport et la présentation du sujet d'étude
