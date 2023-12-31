"Auteur : Mélodie FLEURY"
"Date : Décembre 2023"
"Objectif : Réaliser les jointures nécessaires pour le fichier 'tracks'." 
"Fichier final : 'tracks_join_final_noNA'" 

"ATTENTION: Pour que le code fonctionne, remplacer 'à modifier' par le bon chemin du fichier"

#Chemin absolus d'accès au projet Github
cheminAccesProjetGithub = "E:/COURS ING3/Projet Geostat/" #a modifier
setwd(cheminAccesProjetGithub)

#Librairies utilisées
library(magrittr)
library(dplyr)

#Lecture des différents fichiers csv nécessaires
tracks <- read.csv(
  file = "SpotifyTopSongsGeoStat/Final_Data/tracks_final.csv", #a modifier
  sep = ";"
  )

raw_tracks <- read.csv (
  file = "SpotifyTopSongsGeoStat/data/fma_metadata/raw_tracks.csv", #a modifier
  sep = ","
  )

echonest <- read.csv(
  file = "SpotifyTopSongsGeoStat/Final_Data/echonest.csv", #a modifier
  sep = ";"
  )

#Jointure du fichier "tracks.csv" aux fichiers "raw_tracks.csv" et "echonest.csv"
tracks_join1 <- merge(
  x = tracks,
  y = raw_tracks[,c("track_id", "track_explicit")],
  by = "track_id",
  all.x = TRUE
  )

#Jointure du fichier "tracks_join1" et "echonest.csv"
tracks_join_final <- merge(
  x = tracks_join1,
  y = echonest[,
               c("track_id", "acousticness",
                 "danceability", "energy",
                 "instrumentalness", "liveness",
                 "speechiness", "tempo", "valence")],
  by = "track_id",
  all.x = TRUE
)

#Supprimer les chansons dont une de leurs caractéristiques musicales est vide
tracks_join_final_noNA <- tracks_join_final %>% filter(
  !is.na(acousticness) & 
  !is.na(danceability) &
  !is.na(energy) &
  !is.na(instrumentalness) &
  !is.na(liveness) &
  !is.na(speechiness) &
  !is.na(tempo) &
  !is.na(valence)
)

# Sauvegarder le dataframe "tracks_join_final_noNA" en tant que fichier CSV
write.csv(tracks_join_final_noNA, file = "tracks_final_noNA.csv", row.names = FALSE)