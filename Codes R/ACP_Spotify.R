"Auteur : Fosio ULUTUIPALELEI"
"Date : Decembre 2023"
"Objectif : Realiser la méthode ACP sur les données du dataset Spotify"

"ATTENTION: Pour que le code fonctionne, remplacer 'à modifier' par le bon chemin du fichier"

install.packages("ade4")
install.packages("factoextra")
install.packages("dplyr")
install.packages("ggplot2")

library(ade4)
library(ggplot2)
library(factoextra)
library(dplyr)

music_spotify <-  read.csv(
  "data/universal_top_spotify_songs/universal_top_spotify_songs_Oct_18_28.csv"
  ) #à modifier

music_spotify <- filter(music_spotify, country != '')
music_spotify <- music_spotify[1:1000,]

# Selectionner 1000 lignes au hasard
#indices_aleatoires <- sample(nrow(music_spotify), 1000, replace = FALSE)
#music_spotify_1000 <- music_spotify[indices_aleatoires, ]

# sélection des variables numériques
music_spotify_num <- select(music_spotify, where(is.numeric))

music_spotify_num <- music_spotify_num %>% select(
  -daily_movement, 
  -weekly_movement, 
  -time_signature, 
  -daily_rank)

music_spotify_num <- music_spotify_num %>% select(-loudness,
                                                  -duration_ms,
                                                  -tempo,
                                                  -key,
                                                  -mode)

#Affichage de l'histogramme 
for (n in names(music_spotify_num)) {
  print(n)
  DonnéesSpotify <- pull(music_spotify_num, n)
  hist(DonnéesSpotify, main = paste("Histogramme de l'attribut", n), xlab = n)
}

music_spotify_num <- filter(music_spotify_num, popularity > 40 &
                        duration_ms < 350000 & duration_ms > 100000 &
                        danceability > 0.4 &
                        energy > 0.3 &
                        loudness > -13 &
                        speechiness < 0.4 &
                        instrumentalness < 0.1 &
                        liveness < 0.5 &
                        valence > 0.1 &
                        tempo > 70 & tempo < 180)

#avoir des infos statistiques sur les données
#summary(music_spotify)

#ACP
dataACP <- music_spotify_num

# matrice variance covariance
#mat_var_cov <- var(dataACP)
#mat_var_cov

# corrélation
mat_cor <- cor(dataACP)
mat_cor

ggpairs(dataACP)

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
var(dataACP$danceability)

#Calcul ACP
resultACP <- dudi.pca(df = dataACP, scannf = 'False', nf = 2)

#Valeurs propres
resultACP$eig

#Visualisation des valeurs propres
fviz_eig(resultACP)

get_pca_ind(resultACP)
get_pca_var(resultACP)

fviz_pca_ind(resultACP)

# cercle
fviz_pca_var(resultACP)