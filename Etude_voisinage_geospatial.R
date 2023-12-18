#Chemin absolus d'accès au projet Github
cheminAccesProjetGithub = "E:/COURS ING3/Projet Geostat/" #a modifier
setwd(cheminAccesProjetGithub)

#Librairies utilisées
library(magrittr)
library(dplyr)
library(sf)
library(mapsf)

#Lecture des différents fichiers
tracks <- read.csv(
  "SpotifyTopSongsGeoStat/Final_Data/tracks_final_noNA.csv")

world_boundaries <- st_read(
  "SpotifyTopSongsGeoStat/data/world-administrative-boundaries/world-administrative-boundaries.shp",
  layer = "world-administrative-boundaries",
  quiet = TRUE
)

#Affichage
mf_map(world_boundaries)

#Transformer fichier csv "tracks" en shp
prj4string <- "+proj=longlat + ellps=WGS84 +datum=WGS84 +no_defs"
my.projection <- st_crs(prj4string)

tracks_shp <- st_as_sf(
  tracks, 
  coords = c("longitude_artist", "latitude_artist"), 
  na.fail = FALSE,
  crs = my.projection
  )
st_crs(tracks_shp)

# Affichage     
mf_map(world_boundaries, col = "lemonchiffon2")
mf_map(tracks_shp, col = "#d35f3c", add = TRUE)

#Jointure spatiale entre la localisation des artistes des chansons "tracks_shp" 
# et la localisation des pays/continents de la couche "world_boundaries" avec 
# le prédicat "st_within"
tracks_world <- st_join(
  x = tracks_shp,
  y = world_boundaries[, c("name",
                       "continent",
                       "region",
                       "french_shor")],
  join = st_within,
  left = TRUE
)


#Sélection chansons du Mexique de "tracks_world"
MEX_tracks = tracks_world %>% filter(
  french_shor == "Mexique")

#Sélection chansons de France de "tracks_world"
FR_tracks = tracks_world %>% filter(
  french_shor == "France")

#Sélection chansons des USA de "tracks_world"
USA_tracks = tracks_world %>% filter(
  french_shor == "États-Unis d'Amérique")

#Sélection chansons de Venezuela de "tracks_world"
VEN_tracks = tracks_world %>% filter(
  french_shor == "Venezuela")

#Sélection chansons d'Amerique du Sud de "tracks_world"
S.AM_tracks = tracks_world %>% filter(
  region == "South America")

#Sélection chansons d'Europe de "tracks_world"
EU_tracks = tracks_world %>% filter(
  continent == "Europe")

#Sélection chansons d'Amérique du Nord de "tracks_world"
N.AM_tracks = tracks_world %>% filter(
  region == "Northern America")

#MEXIQUE - FRANCE
#Valence
par(mfrow = c(1, 2))  # 1 ligne, 2 colonnes
plot(MEX_tracks$valence, type = "p", col = "red", main = "Valence Mexique")
mean(MEX_tracks$valence) #0.32

plot(FR_tracks$valence, type = "p", col = "blue", main = "Valence France")
mean(FR_tracks$valence) #0.43

#Tempo 
par(mfrow = c(1, 2))  # 1 ligne, 2 colonnes
plot(MEX_tracks$tempo, type = "p", col = "red", main = "Tempo Mexique")
mean(MEX_tracks$tempo) #123

plot(FR_tracks$tempo, type = "p", col = "blue", main = "Tempo France")
mean(FR_tracks$tempo) #128

#MEXIQUE-VENEZUELA
#Valence
par(mfrow = c(1, 2))  # 1 ligne, 2 colonnes
plot(MEX_tracks$valence, type = "p", col = "red", main = "Valence Mexique")
mean(MEX_tracks$valence) #0.32

plot(VEN_tracks$valence, type = "p", col = "blue", main = "Valence Guatemala")
mean(VEN_tracks$valence) #0.33

#Tempo 
par(mfrow = c(1, 2))  # 1 ligne, 2 colonnes
plot(MEX_tracks$tempo, type = "p", col = "red", main = "Tempo Mexique")
mean(MEX_tracks$tempo) #123

plot(VEN_tracks$tempo, type = "p", col = "blue", main = "Tempo Guatemala")
mean(VEN_tracks$tempo) #140 

# => Pas beaucoup de valeurs pour le vénézuela


