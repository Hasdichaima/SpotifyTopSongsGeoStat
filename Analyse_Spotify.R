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

# le rang 1
music_spotify_40_50 <- filter(music_spotify, daily_rank < 11)
music_spotify_10 <- filter(music_spotify, daily_rank > 39 & daily_rank < 51)

# conversion des milliseconde en seconde
# music_spotify$duration_ms <- music_spotify$duration_ms/(60*1000)


##################################
# SUPRRIMER LES DOUBLONS
##################################

# Spécifiez la colonne pour laquelle vous voulez vérifier les doublons
colonne_specifique <- "name"

# Identifier les lignes dupliquées dans la colonne spécifique
indices_doublons <- duplicated(music_spotify[, colonne_specifique])

# Supprimer les lignes dupliquées
music_spotify_sans_doublons <- music_spotify[!indices_doublons, ]

##################################
##################################

# valeur numérique
music_spotify_sans_doublons <- select(music_spotify_sans_doublons, where(is.numeric))
# filtrage des attributs inutiles pour notre étude
music_spotify_sans_doublons <- music_spotify_sans_doublons %>% select(-daily_movement, -weekly_movement, -time_signature)
music_spotify_sans_doublons <- music_spotify_sans_doublons %>% select(-daily_rank, -popularity)
summary(music_spotify_sans_doublons)

##################################
# TOP 10
##################################

# sélection des variables numériques
music_spotify_num_10 <- select(music_spotify_10, where(is.numeric))
# filtrage des attributs inutiles pour notre étude
music_spotify_num_10 <- music_spotify_num_10 %>% select(-daily_movement, -weekly_movement, -time_signature)
music_spotify_num_10 <- music_spotify_num_10 %>% select(-daily_rank, -popularity)
music_spotify_num_10 <- music_spotify_num_10 %>% select(-duration_ms, -key, -loudness, -mode)
music_spotify_num_10 <- music_spotify_num_10 %>% select(-tempo)

##################################
# TOP 40 - 50
##################################

# sélection des variables numériques
music_spotify_num_40 <- select(music_spotify_40_50, where(is.numeric))
# filtrage des attributs inutiles pour notre étude
music_spotify_num_40 <- music_spotify_num_40 %>% select(-daily_movement, -weekly_movement, -time_signature)
music_spotify_num_40 <- music_spotify_num_40 %>% select(-daily_rank, -popularity)
music_spotify_num_40 <- music_spotify_num_40 %>% select(-duration_ms, -key, -loudness, -mode, -tempo)

##################################
# EN GENERAL
##################################

# sélection des variables numériques
music_spotify_num <- select(music_spotify, where(is.numeric))
# filtrage des attributs inutiles pour notre étude
music_spotify_num <- music_spotify_num %>% select(-daily_movement, -weekly_movement, -time_signature)
music_spotify_num <- music_spotify_num %>% select(-daily_rank, -popularity)

summary(music_spotify_num)


# popularité supérieur à 40
#music_spotify <- filter(music_spotify, popularity > 40 &
#                         danceability > 0.5 &
#                        loudness > -10 &
#                       instrumentalness < 0.1 &
#                      liveness < 0.4 &
#                     country != '' &
#                    speechiness < 0.4 &
#                   duration_ms < 350000 & duration_ms > 100000 &
#                  energy > 0.3 &
#                 valence > 0.1 &
#                tempo > 70 & tempo < 180 &
#               dailyrank == 1)

#popularité entre 50 et 70
#music_spotify <- filter(music_spotify, popularity > 50 & popularity < 70 &
#                          country != '' &
#                         speechiness < 0.4 &
#                        duration_ms < 350000 &
#                       energy > 0.2 &
#                      tempo > 50 & tempo < 200 )


#avoir des infos statistiques sur les données
#summary(music_spotify)

### GRAPHIQUE EN RADAR ###

#dataRadar <- music_spotify_sans_doublons
#dataRadar <- music_spotify_num
dataRadar10 <- music_spotify_num_10
dataRadar40 <- music_spotify_num_40

#fonction qui centre et rédut un vecteur de variables
center_reduce <- function(x){
  avg <- mean(x, na.rm = TRUE)
  ET <- sd(x)
  return((x-avg)/ET)
}

for (colonne in colnames(dataRadar)) {
  dataRadar[[colonne]] <- center_reduce(dataRadar[[colonne]])
}

# Initialiser une liste pour stocker les médianes
medianes_liste <- list()
medianes_liste_10 <- list()
medianes_liste_40 <- list()

# Boucle à travers chaque colonne du data frame
for (colonne in names(dataRadar10)) {
  # Calculer la médiane de la colonne actuelle
  mediane_colonne <- median(dataRadar10[[colonne]])
  
  # Stocker la médiane dans la liste
  medianes_liste_10[[colonne]] <- mediane_colonne
}

# Boucle à travers chaque colonne du data frame
for (colonne in names(dataRadar40)) {
  # Calculer la médiane de la colonne actuelle
  mediane_colonne <- median(dataRadar40[[colonne]])
  
  # Stocker la médiane dans la liste
  medianes_liste_40[[colonne]] <- mediane_colonne
}

mon_data_frame10 <- data.frame(medianes_liste_10)
mon_data_frame40 <- data.frame(medianes_liste_40)

mon_data_frame10 <- rbind(rep(1,7) , rep(0,7) , mon_data_frame10)
mon_data_frame40 <- rbind(rep(1,7) , rep(0,7) , mon_data_frame40)

# Créer le graphique en radar
radar1 <- radarchart(mon_data_frame10, caxislabels = c(0, 0.25, 0.5, 0.75, 1), axistype = 1,
                     axislabcol = "black")
radar2 <- radarchart(mon_data_frame40, linecol = "red", pcol = "red")
par(new = TRUE)

legend("topleft", legend = "Top 10", col = "black", lty = 1)
legend("topright", legend = "Top 40 - 50", col = "red", lty = 1)

