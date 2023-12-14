
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
#---------------------------------------- A CONTINUER ----------------------------------
# nuage de points 2 à 2
plot(data_penguins_num)

# matrice de corrélation des 4 variables
cor(data_penguins_num)

# histogramme (cas bcp variables)
for (n in names(data_penguins_num)) {
  lili <- pull(data_penguins_num, n)
  hist(lili)
}

# Correlogramme des variables numérique du dataset penguins
ggpairs(data_penguins_num)
# => On remarque de bonnes corrélations entre les variables 
# et des nuages de points où la RL peut y être appliqués 
# et d'autres sous formes de groupes.
# il y a deux modalités pour la flipper_length

## Q3 : Préparation des données
#Sélection variables numériques liées à la morpho des animaux
dataACP <- data_penguins_num
View(dataACP)

## Q4
mat_var_cov <- var(dataACP)
mat_var_cov
#sur la diagonale, les variances individuelles 
# sinon covariance entre les deux variables

#extraction de la diagonale
didi <- diag(mat_var_cov)
#somme des variances
sum(didi)

##Q4 bis
#fonction qui centre et rédut un vecteur de variables
center_reduce <- function(x){
  avg <- mean(x, na.rm = TRUE)
  ET <- sd(x)
  return((x-avg)/ET)
}

#test fonction "center_reduce"
center_reduce(c(1,2,3,58))

#calcul de la var d'une variable centrée & réduite
bill_length_reduite <- center_reduce(dataACP$bill_length_mm)
var(bill_length_reduite)
# => Cela vaut 1

## Q5
#Calcul ACP
resultACP <- dudi.pca(dataACP)
resultACP <- dudi.pca(dataACP, nf = 2, scannf = FALSE)
resultACP

#Valeurs propres
resultACP$eig
#inertie
inertietot <- sum(resultACP$eig)
#part d'inertie
# ... à compléter

#Extraction valeurs propres/variances composantes principales
get_eigenvalue(resultACP)
#Visualisation des valeurs propres
fviz_eig(resultACP)

get_pca_ind(resultACP)
get_pca_var(resultACP)

fviz_pca_ind(resultACP)
fviz_pca_var(resultACP)
