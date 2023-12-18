#Librairies utilisées
library(magrittr)
library(dplyr)
library(GGally)
library(ggplot2)
library(ade4)
library(factoextra)

#Lecture fichier csv 
tracks_final_noNA = read.csv(
  "E:/COURS ING3/Projet Geostat/SpotifyTopSongsGeoStat/Final_Data/tracks_final_noNA.csv")

#On sélectionne les 1000 premières lignes
subset_tracks_final_noNA = tracks_final_noNA[1:1000,]

#nom et type de données du dataset
str(subset_tracks_final_noNA)

#avoir des infos statistiques sur les données (quartiles, médiane, moyenne pour chaque attribut...)
summary(subset_tracks_final_noNA)

#Sélection des variables numériques
tracks_final_noNA_num <- select(subset_tracks_final_noNA, where(is.numeric))
head(tracks_final_noNA_num)

#Suppression des variables "track_id", "latitude_artist", "longitude_artist"
tracks_final_noNA_num <- tracks_final_noNA_num %>% 
  select(
    -track_id, 
    -latitude_artist,
    -longitude_artist)

#Tri des entités selon leur répartition par attributs dans les histogrammes
for (n in names(tracks_final_noNA_num)) {
  print(n)
  DonnéesFMA <- pull(tracks_final_noNA_num, n)
  hist(DonnéesFMA, main = n)
}

tracks_selection <- tracks_final_noNA_num %>% 
  filter(
    duration < 600 &
    favorites <= 25 &
    listens <= 20000 &
    tempo >= 40 & tempo <= 225 & 
    speechiness < 0.2 &
    liveness <= 0.4 &
    danceability >= 0.1 & danceability <= 0.7
    )

# => On remarque que la majorité des chansons ne sont pas écoutées plus de 20 000 fois
# donc tri nécessaire 
# => On remarque que la majorité des chansons ne sont pas mis en favoris plus de 25 fois

#Visualiser les données résultantes
View(tracks_selection)

# nuage de points 2 à 2
plot(tracks_selection)

# matrice de corrélation des variables
cor(tracks_selection)

#les corrélations les plus pertinentes sont : 
# listens - favorites (0.65)
# duration - favorites (0.15)
# duration - energy (0.18)
# duration - tempo (0.15)
# duration - valence (0.27)
# acousticness - listens (0.1)
# acousticness - energy (0.43)
# acousticness - tempo (0.25)
# danceability - energy (0.12)
# danceability - instrumentalness (0.14)
# danceability - tempo (0.15)
# danceability - valence (0.42)
# energy - instrumentalness (0.13)
# energy - liveness (0.12)
# energy - speechiness (0.20)
# energy - tempo (0.31)
# energy - valence (0.20)

plot(x = tracks_selection$valence,
     y = tracks_selection$tempo)

#RL = lm(tracks_selection$valence ~ tracks_selection$tempo) #Var à expliquer ~ Var explicative
#plot(RL)
#summary(RL)

# Correlogramme des variables numérique du dataset FMA
ggpairs(tracks_selection)

#------------------------------------------------------------------------------------
#dataACP <- tracks_selection
#View(dataACP)

#mat_var_cov <- var(dataACP)
#mat_var_cov
#sur la diagonale, les variances individuelles 
# sinon covariance entre les deux variables

#extraction de la diagonale
#didi <- diag(mat_var_cov)
#somme des variances
#sum(didi)

#normalisation
center_reduce <- function(x){
  avg <- mean(x, na.rm = TRUE)
  ET <- sd(x)
  return((x-avg)/ET)
}

for (colonne in colnames(tracks_selection)){
  tracks_selection[[colonne]] <- center_reduce(tracks_selection[[colonne]])
}

#Calcul ACP
resultACP <- dudi.pca(dataACP)
resultACP <- dudi.pca(dataACP, nf = 2, scannf = FALSE, scale = T )
resultACP

#Valeurs propres
resultACP$eig
#inertie
inertietot <- sum(resultACP$eig)
inertietot

#Extraction valeurs propres/variances composantes principales
get_eigenvalue(resultACP)
#Visualisation des valeurs propres (Scree plot)
fviz_eig(resultACP)

get_pca_ind(resultACP)
get_pca_var(resultACP)

fviz_pca_ind(resultACP)
fviz_pca_var(resultACP)
