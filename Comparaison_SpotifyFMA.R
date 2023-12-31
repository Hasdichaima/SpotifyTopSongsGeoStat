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

music_spotify <-  read.csv("data/universal_top_spotify_songs/universal_top_spotify_songs_Oct_18_28.csv")
music_spotify <- filter(music_spotify, country != '')


# sélection des variables numériques
music_spotify_num <- select(music_spotify, where(is.numeric))

# filtrage des attributs inutiles pour notre étude
music_spotify_num <- music_spotify_num %>% select(-daily_movement, -weekly_movement, -time_signature)
music_spotify_num <- music_spotify_num %>% select(
  -daily_rank,
  -duration_ms, 
  -key,
  -loudness,
  -mode,
  -popularity,
  -tempo
)


#################################################
# PREPARATION DONNEES POUR GRAPHIQUE RADAR MEDIAN
#################################################

################### 
#SPOTIFY
###################
dataRadar <- music_spotify_num

# Initialiser une liste pour stocker les médianes
medianes_liste <- list()

# Boucle à travers chaque colonne du data frame
for (colonne in names(dataRadar)) {
  # Calculer la médiane de la colonne actuelle
  mediane_colonne <- median(dataRadar[[colonne]])
  # Stocker la médiane dans la liste
  medianes_liste[[colonne]] <- mediane_colonne
}

# transforme en data frame
mon_data_frame <- data.frame(medianes_liste)

# création de deux lignes (min et max) pour le graphique en radar
max_min <- matrix(c(1,0), nrow = 2, ncol = ncol(mon_data_frame))
colnames(max_min) <- names(mon_data_frame)
rownames(max_min) <- c("Max", "Min")

# on ajoute les deux lignes min et max
mon_data_frame <- rbind(max_min, mon_data_frame)

################### 
#FMA
###################

tracks_final_noNA = read.csv(
  "Final_Data/tracks_final_noNA.csv")

tracks_final_noNA = read.csv("/Users/fosio/Desktop/GeoDataScience/projet_stat/SpotifyTopSongsGeoStat/Final_Data/tracks_final_noNA.csv")
#Selection des variables numeriques
tracks_final_noNA_num <- select(tracks_final_noNA, where(is.numeric))

#Suppression des variables "track_id", "latitude_artist", "longitude_artist"
tracks_final_noNA_num <- tracks_final_noNA_num %>% 
  select(
    -track_id, 
    -latitude_artist,
    -longitude_artist)


#Suppression des variables "listens" et "favorites" car pas possible de les comparer
# aux attributs musicales

subtracks_radarchart <- tracks_final_noNA_num %>% 
  select(
    -listens,
    -favorites,
    -tempo,
    -duration)

df_mediane <- subtracks_radarchart %>% 
  summarise(
    across(everything(), median))
rownames(df_mediane) <- "Mediane"

max_min <- matrix(c(1,0), nrow = 2, ncol = ncol(subtracks_radarchart))
colnames(max_min) <- names(subtracks_radarchart)
rownames(max_min) <- c("Max", "Min")

df <- rbind(max_min, df_mediane)

# on modifie l'ordre des colonnes pour qu'il correspond à l'ordre de "mon dataframe"
df <- df %>%
  select("danceability", "energy", "speechiness", "acousticness", "instrumentalness", "liveness", "valence")

#############################
# Créer le graphique en radar
#############################

# radarchart Spotify
radarchart(mon_data_frame, caxislabels = c(0,0.25,0.5,0.75,1),
           axistype = 1,
           # Personnaliser le polygone
           pcol = 'blue', 
           #pfcol = scales::alpha('blue', 0.5), 
           #pfcol = scales::alpha('red', 0.5), 
           plwd = 2, plty = 1,
           # Personnaliser la grille
           cglcol = 'grey', cglty = 1, cglwd = 0.8,
           # Personnaliser l'axe
           axislabcol = "grey"
)

par(new = TRUE)

# radarchart FMA
radarchart(df, caxislabels = c(0,0.25,0.5,0.75,1),
           axistype = 1,
           # Personnaliser le polygone
           pcol = 'red', 
           #pfcol = scales::alpha('red', 0.5), 
           plwd = 2, plty = 1,
           # Personnaliser la grille
           cglcol = 'grey', cglty = 1, cglwd = 0.8,
           # Personnaliser l'axe
           axislabcol = "grey"
           # Étiquettes des variables
           #vlcex = 'test', vlabels = 'test1'
           #title = 'Profil des chansons FMA')
)

legend("topleft", legend = "Spotify", col = "blue", lty = 1)
legend(x="topright", legend=c("FMA"), col=c("red"), lty = 1)





