
setwd("E:/COURS ING3/Projet Geostat/SpotifyTopSongsGeoStat") #a modifier 

#Librairies utilisees
library(magrittr)
library(dplyr)
library(GGally)
library(ggplot2)
library(ade4)
library(factoextra)
library(fmsb)
library(ggbasic) #devtools::install_github("brandon-mosqueda/ggbasic")
library(sf)
library(mapsf)

#Lecture fichier csv 

tracks_final_noNA = read.csv(
  "Final_Data/tracks_final_noNA.csv")

world_boundaries <- st_read(
  "data/world-administrative-boundaries/world-administrative-boundaries.shp",
  layer = "world-administrative-boundaries",
  quiet = TRUE
)

#nom et type de données du dataset
str(tracks_final_noNA)

#Visualisation des premieres valeurs des différentes variables
head(tracks_final_noNA)

#Selection des variables numeriques
tracks_final_noNA_num <- select(tracks_final_noNA, where(is.numeric))
head(tracks_final_noNA_num)

#Suppression des variables "track_id", "latitude_artist", "longitude_artist"
tracks_final_noNA_num <- tracks_final_noNA_num %>% 
  select(
    -track_id, 
    -latitude_artist,
    -longitude_artist)

#Affichage de l'histogramme de tous les attributs numeriques (11)
for (n in names(tracks_final_noNA_num)) {
  print(n)
  DonneesFMA <- pull(tracks_final_noNA_num, n)
  hist(DonneesFMA, main = paste("Histogramme de l'attribut", n), xlab = n)
}

#avoir des infos statistiques sur les données (quartiles, médiane, moyenne pour chaque attribut...)
summary(tracks_final_noNA_num)

#Visualiser les données résultantes
View(tracks_final_noNA_num)

# nuage de points 2 à 2
plot(tracks_final_noNA_num)

# matrice de corrélation des variables
cor(tracks_final_noNA_num)

# Correlogramme des variables numérique du dataset FMA
ggpairs(tracks_final_noNA_num)

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
cor.test(tracks_final_noNA_num$listens, tracks_final_noNA_num$favorites) 
#Correlation = 0.61 et intervalle de confiance entre 0.56 et 0.65

plot(x = tracks_final_noNA_num$listens, xlab = "Nombre d'écoutes",
     y = tracks_final_noNA_num$favorites, ylab = "Nombre de mises en favoris")

RL_LF = lm(tracks_final_noNA_num$favorites ~ tracks_final_noNA_num$listens) #Var explicative ~ Var à expliquer
summary(RL_LF)
abline(RL_LF, col = 'red')
#p-value bonne car p-value < 2.2e-16 et R² (coefficient de détermination linéaire) = 0.37 donc
# qualité de la régression assez faible 

#RADARCHART MEDIANE DE TOUS LES ATTRIBUTS NUMERIQUES DES CHANSONS FMA

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

radarchart(df, caxislabels = c(0,0.25,0.5,0.75,1),
           axistype = 1,
           # Personnaliser le polygone
           pcol = 'red', 
           pfcol = scales::alpha('red', 0.5), 
           plwd = 2, plty = 1,
           # Personnaliser la grille
           cglcol = 'grey', cglty = 1, cglwd = 0.8,
           # Personnaliser l'axe
           axislabcol = "grey", 
           # Étiquettes des variables
           #vlcex = 'test', vlabels = 'test1',
           title = 'Profil des chansons FMA')


#DIAGRAMME EN BARRES ENTRE GENRE MUSCIALE ET EXPLICIT

tracks_explicit <- tracks_final_noNA %>% 
  filter (track_explicit == 'Radio-Unsafe') %>% 
  group_by(genre_top) %>%
  summarise(Perc_explicit_tracks = n()) %>%
  mutate(Perc_explicit_tracks = round(Perc_explicit_tracks/sum(Perc_explicit_tracks)*100, 2)) %>%
  mutate(genre_top = ifelse(genre_top == "", "Autre", genre_top))

View(tracks_explicit)

bar_plot(tracks_explicit, x = genre_top, x_lab = "Genre musicale", 
         y = Perc_explicit_tracks, y_lab = "Pourcentage de chansons explicit",
         x_angle = 45)

#LIEN ENTRE CONTINENT ET CARACTÉRISTIQUES MUSICALES

#Transformer fichier csv "tracks" en shp
prj4string <- "+proj=longlat + ellps=WGS84 +datum=WGS84 +no_defs"
my.projection <- st_crs(prj4string)

tracks_shp <- st_as_sf(
  tracks_final_noNA, 
  coords = c("longitude_artist", "latitude_artist"), 
  na.fail = FALSE,
  crs = my.projection
)
st_crs(tracks_shp)


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

View(tracks_world)

tracks_median_continent <- tracks_world %>% 
  as.data.frame() %>%
    group_by(continent) %>% 
  summarise_at(
    c("acousticness", 
      "valence", 
      "speechiness",
      "liveness",
      "instrumentalness",
      "energy",
      "danceability"), ~round(median(.),2)
    ) %>% 
  mutate(continent = ifelse(is.na(continent), "Other", continent)) %>%
  select(-continent)
  
View(tracks_median_continent)

max_min <- matrix(c(1,0), nrow = 2, ncol = ncol(tracks_median_continent))
colnames(max_min) <- names(tracks_median_continent)
rownames(max_min) <- c("Max", "Min")
rownames(tracks_median_continent) <- c("Afrique", "Amérique", "Asie", "Europe", "Océanie", "Autre")
df_continent <- rbind(max_min, tracks_median_continent)
View(df_continent)

radarchart(df_continent, caxislabels = c(0,0.25,0.5,0.75,1),
           axistype = 1,
           # Personnaliser le polygone
           pcol = c('#FF0000','#00FF00', '#0000FF','#FFFF00', '#800080', '#40E0D0'),
           #pfcol = scales::alpha(, 0.5), 
           plwd = 2, plty = 1,
           # Personnaliser la grille
           cglcol = 'grey', cglty = 1, cglwd = 1,
           # Personnaliser l'axe
           axislabcol = "grey")

legend(
  x = "right", y = "bottom", legend = rownames(df_continent[-c(1,2),]), horiz = FALSE,
  bty = "n", pch = 20 , col = c(
    '#FF0000','#00FF00', '#0000FF','#FFFF00', '#800080', '#40E0D0' ),
  text.col = "black", cex = 1, pt.cex = 1.5
)

#On se rend compte ici que certains histogrammes sont écrasés car les données sont
#trop hétérogènes. Il faut alors créer un sous jeu de données plus homogènes pour l'ACP.

#Création du subset pour l'ACP
#On sélectionne les 1000 premières lignes
subset_tracks_final_noNA = tracks_final_noNA_num[1:1000,]

#Tri des entités selon leur répartition par attributs dans les histogrammes
for (n in names(subset_tracks_final_noNA)) {
  print(n)
  DonnéesFMA <- pull(subset_tracks_final_noNA, n)
  hist(DonnéesFMA, main = n)
}

tracks_selection <- subset_tracks_final_noNA %>% 
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

dataACP <- tracks_selection
View(dataACP)

# Fonction de normalisation des données
center_reduce <- function(x){
  avg <- mean(x, na.rm = TRUE)
  ET <- sd(x)
  return((x-avg)/ET)
}

#Normalisation des données avant de faire l'ACP
for (colonne in colnames(dataACP)){
  dataACP[[colonne]] <- abs(center_reduce(dataACP[[colonne]]))
}

#Calcul ACP
resultACP <- dudi.pca(dataACP, nf = 2, scannf = FALSE, scale = T )
resultACP

#Valeurs propres
resultACP$eig
#inertie
inertietot <- sum(resultACP$eig)
inertietot #11

#Extraction valeurs propres/variances composantes principales
get_eigenvalue(resultACP)

#Visualisation des valeurs propres (Scree plot)
fviz_eig(resultACP)

#Individuals - PCA
fviz_pca_ind(resultACP)

#Cercle de corrélation
fviz_pca_var(resultACP)
