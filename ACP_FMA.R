#Librairies utilisées
library(magrittr)
library(dplyr)
library(GGally)
library(ggplot2)
library(ade4)
library(factoextra)

#Lecture fichier csv 
tracks_final_noNA = read.csv(
  "E:/COURS ING3/Projet Geostat/SpotifyTopSongsGeoStat/Final_Data/tracks_final_noNA.csv")

#nom et type de données du dataset
str(tracks_final_noNA)

#avoir des infos statistiques sur les données
summary(tracks_final_noNA)

#Sélection des variables numériques
tracks_final_noNA_num <- select(tracks_final_noNA, where(is.numeric))
head(tracks_final_noNA_num)

#Suppression des variables "track_id", "latitude_artist", "longitude_artist"
tracks_final_noNA_num <- tracks_final_noNA_num %>% 
  select(
    -track_id, 
    -latitude_artist,
    -longitude_artist)

#Visualiser les données résultantes
View(tracks_final_noNA_num)

# nuage de points 2 à 2
plot(tracks_final_noNA_num)

# matrice de corrélation des variables
cor(tracks_final_noNA_num)

# histogramme (cas bcp variables)
for (n in names(tracks_final_noNA_num)) {
  lili <- pull(tracks_final_noNA_num, n)
  hist(lili)
}

# Correlogramme des variables numérique du dataset FMA
ggpairs(tracks_final_noNA_num)

dataACP <- tracks_final_noNA_num
View(dataACP)

mat_var_cov <- var(dataACP)
mat_var_cov
#sur la diagonale, les variances individuelles 
# sinon covariance entre les deux variables

#extraction de la diagonale
didi <- diag(mat_var_cov)
#somme des variances
sum(didi)

#Calcul ACP
resultACP <- dudi.pca(dataACP)
resultACP <- dudi.pca(dataACP, nf = 3, scannf = FALSE, scale = T )
resultACP

#Valeurs propres
resultACP$eig
#inertie
inertietot <- sum(resultACP$eig)
inertietot

#Extraction valeurs propres/variances composantes principales
get_eigenvalue(resultACP)
#Visualisation des valeurs propres
fviz_eig(resultACP)

get_pca_ind(resultACP)
get_pca_var(resultACP)

fviz_pca_ind(resultACP)
fviz_pca_var(resultACP)
