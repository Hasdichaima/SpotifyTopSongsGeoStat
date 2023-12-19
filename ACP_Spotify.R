install.packages("ade4")
install.packages("factoextra")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("fmsb")

library(ade4)
library(ggplot2)
library(factoextra)
library(dplyr)
library(fmsb)

#Histogramme

# a voir
#hist(music_spotify$danceability)
#hist(music_spotify$loudness)
#hist(music_spotify$instrumentalness)
#hist(music_spotify$liveness)
#hist(music_spotify$popularity)
#hist(music_spotify$duration_ms)
#hist(music_spotify$energy)
#hist(music_spotify$key)
#hist(music_spotify$mode)
#hist(music_spotify$speechiness)
#hist(music_spotify$acousticness)
#hist(music_spotify$valence)
#hist(music_spotify$tempo)

music_spotify <-  read.csv("/Users/fosio/Desktop/GeoDataScience/projet_stat/universal_top_spotify_songs.csv")

# sélection des variables numériques
music_spotify_num <- select(music_spotify, where(is.numeric))
# filtrage des attributs inutiles pour notre étude
music_spotify_num <- music_spotify_num %>% select(-daily_movement, -weekly_movement, -time_signature, -daily_rank)


#avoir des infos statistiques sur les données
#summary(music_spotify)

#########################

#ACP

dataACP <- music_spotify_num
View(dataACP)

# matrice variance covariance
#mat_var_cov <- var(dataACP)
#mat_var_cov

# corrélation
mat_cor <- cor(dataACP)
mat_cor

#fonction qui centre et rédut un vecteur de variables
center_reduce <- function(x){
  avg <- mean(x, na.rm = TRUE)
  ET <- sd(x)
  return((x-avg)/ET)
}

for (colonne in colnames(dataACP)) {
  dataACP[[colonne]] <- center_reduce(dataACP[[colonne]])
}


#test si c'est égale à 1
var(dataACP$popularity)

#Calcul ACP
resultACP <- dudi.pca(df = dataACP, scannf = 'False', nf = 4)

#Valeurs propres
resultACP$eig

#Visualisation des valeurs propres
fviz_eig(resultACP)

get_pca_ind(resultACP)
get_pca_var(resultACP)

fviz_pca_ind(resultACP)

# cercle
fviz_pca_var(resultACP)


