
setwd("E:/COURS ING3/Projet Geostat/SpotifyTopSongsGeoStat")

#Librairies utilisées
library(magrittr)
library(dplyr)
library(GGally)
library(ggplot2)
library(ade4)
library(factoextra)
library(fmsb)

#Lecture fichier csv 
tracks_final_noNA = read.csv(
  "E:/COURS ING3/Projet Geostat/SpotifyTopSongsGeoStat/Final_Data/tracks_final_noNA.csv")

#Sélection des variables numériques
tracks_final_noNA_num <- select(tracks_final_noNA, where(is.numeric))
head(tracks_final_noNA_num)

#Suppression des variables "track_id", "latitude_artist", "longitude_artist"
tracks_final_noNA_num <- tracks_final_noNA_num %>% 
  select(
    -track_id, 
    -latitude_artist,
    -longitude_artist)

#Affichage de l'histogramme de tous les attributs numériques (11)
for (n in names(tracks_final_noNA_num)) {
  print(n)
  DonnéesFMA <- pull(tracks_final_noNA_num, n)
  hist(DonnéesFMA, main = paste("Histogramme de l'attribut", n), xlab = n)
}
#On se rend compte ici que certains histogrammes sont écrasés car les données sont
#trop hétérogènes. Il faut alors créer un sous jeu de données plus homogènes.


#Création du subset 
#On sélectionne les 1000 premières lignes
subset_tracks_final_noNA = tracks_final_noNA[1:1000,]

#nom et type de données du dataset
str(subset_tracks_final_noNA)

#avoir des infos statistiques sur les données (quartiles, médiane, moyenne pour chaque attribut...)
summary(subset_tracks_final_noNA)

#Sélection des variables numériques
subtracks_final_noNA_num <- select(subset_tracks_final_noNA, where(is.numeric))
head(subtracks_final_noNA_num)

#Suppression des variables "track_id", "latitude_artist", "longitude_artist"
subtracks_final_noNA_num <- subtracks_final_noNA_num %>% 
  select(
    -track_id, 
    -latitude_artist,
    -longitude_artist)

#Tri des entités selon leur répartition par attributs dans les histogrammes
for (n in names(subtracks_final_noNA_num)) {
  print(n)
  DonnéesFMA <- pull(subtracks_final_noNA_num, n)
  hist(DonnéesFMA, main = n)
}

tracks_selection <- subtracks_final_noNA_num %>% 
  filter(
    duration < 600 &
    favorites <= 25 &
      listens <= 4000 &
    #listens <= 20000 &
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

# Correlogramme des variables numérique du dataset FMA
ggpairs(tracks_selection)

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
# instrumentalness - valence (0.15)

# Lien (évident) entre les attributs "listens" et "favorites"
cor.test(tracks_selection$listens, tracks_selection$favorites) 
#Correlation = 0.61 et intervalle de confiance entre 0.56 et 0.65

plot(x = tracks_selection$listens, xlab = "Nombre d'écoutes",
     y = tracks_selection$favorites, ylab = "Nombre de mises en favoris")

RL_LF = lm(tracks_selection$favorites ~ tracks_selection$listens) #Var explicative ~ Var à expliquer
summary(RL_LF)
abline(RL_LF, col = 'red')
#p-value bonne car p-value < 2.2e-16 et R² (coefficient de détermination linéaire) = 0.37 donc
# qualité de la régression assez faible 

#normalisation
center_reduce <- function(x){
  avg <- mean(x, na.rm = TRUE)
  ET <- sd(x)
  return((x-avg)/ET)
}

for (colonne in colnames(tracks_selection)){
  tracks_selection[[colonne]] <- abs(center_reduce(tracks_selection[[colonne]]))
}

df_mediane <- tracks_selection %>% 
  summarise(
    across(everything(), median))
rownames(df_mediane) <- "Mediane"

max_min <- matrix(c(1,0), nrow = 2, ncol = ncol(tracks_selection))
colnames(max_min) <- names(tracks_selection)
rownames(max_min) <- c("Max", "Min")

df <- rbind(max_min, df_mediane)

df_final <- df[c("Mediane", )]
radarchart(df)


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



dataACP <- tracks_selection
#Calcul ACP
resultACP <- dudi.pca(dataACP, nf = 3, scannf = FALSE, scale = T )
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
