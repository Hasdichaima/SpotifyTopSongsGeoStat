install.packages("ade4")
install.packages("factoextra")
install.packages("dplyr")
install.packages("ggplot2")


library(ade4)
library(ggplot2)
library(factoextra)
library(dplyr)

music_spotify <-  read.csv("/Users/fosio/Desktop/GeoDataScience/projet_stat/universal_top_spotify_songs.csv")

#Histogramme

# a voir
hist(music_spotify$danceability)
hist(music_spotify$loudness)
hist(music_spotify$instrumentalness)
hist(music_spotify$liveness)

# popularité supérieur à 40
music_spotify <- filter(music_spotify, popularity > 40 &
                          country != '' &
                          speechiness < 0.4 &
                          duration_ms < 350000 &
                          energy > 0.2 &
                          tempo > 50 & tempo < 200 )

#popularité entre 50 et 70
#music_spotify <- filter(music_spotify, popularity > 50 & popularity < 70 &
#                          country != '' &
 #                         speechiness < 0.4 &
  #                        duration_ms < 350000 &
   #                       energy > 0.2 &
    #                      tempo > 50 & tempo < 200 )


#avoir des infos statistiques sur les données
#summary(music_spotify)

# sélection des variables numériques
music_spotify_num <- select(music_spotify, where(is.numeric))
View(music_spotify)
head(music_spotify_num)

#suppression des variables pas intéréssantes pour notre étude 
music_spotify_num <- music_spotify_num %>% select(-daily_movement, -weekly_movement, -time_signature, -daily_rank)
music_spotify_num <- music_spotify_num %>% select(-mode, -key, -instrumentalness)
#music_spotify_num <- music_spotify_num %>% select(-instrumentalness, -liveness, -duration_ms)

head(music_spotify_num)
View(music_spotify_num)

new_data <- music_spotify_num[1:1000,]
hist(new_data$popularity)
ggpairs(new_data)


#ACP
dataACP <- music_spotify_num
dataACP <- new_data
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
var(dataACP$danceability)

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


