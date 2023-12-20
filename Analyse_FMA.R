
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
subset_tracks_final_noNA = tracks_final_noNA_num[1:1000,]

#nom et type de données du dataset
str(subset_tracks_final_noNA)

#avoir des infos statistiques sur les données (quartiles, médiane, moyenne pour chaque attribut...)
summary(subset_tracks_final_noNA)

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

#RADARCHART MEDIANE DE TOUS LES ATTRIBUTS NUMERIQUES DES CHANSONS FMA

#Suppression des variables "listens" et "favorites" car pas possible de les comparer
# aux attributs musicales

subtracks_radarchart <- subset_tracks_final_noNA %>% 
  select(
    -listens,
    -favorites)

#normalisation
center_reduce <- function(x){
  avg <- mean(x, na.rm = TRUE)
  ET <- sd(x)
  return((x-avg)/ET)
}

for (colonne in colnames(subtracks_radarchart)){
  subtracks_radarchart[[colonne]] <- abs(center_reduce(subtracks_radarchart[[colonne]]))
}

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
