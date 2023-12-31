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

music_spotify <-  read.csv("Final_Data/spotify_ISO_IDH.csv")
music_spotify <- filter(music_spotify, country != '')


# Utilisation de table() pour obtenir les fréquences
table_frequencies <- table(music_spotify$name)
# Trouver l'élément le plus fréquent
element_plus_frequent <- names(table_frequencies)[which.max(table_frequencies)]

# ajout continent des pays
continentMonde <- read.csv('data/continents.csv', sep=';')
resultat <- merge(music_spotify, continentMonde[, c("alpha3", "Continent")], by = "alpha3")

# Copie de la colonne "continent" dans le dataframe "paysSpotify"
music_spotify <- resultat
music_spotify <- music_spotify %>% select(-daily_movement, -weekly_movement, -time_signature)
music_spotify <- music_spotify %>% select(-daily_rank,
                                          -duration_ms, 
                                          -key,
                                          -loudness,
                                          -mode,
                                          -popularity,
                                          -tempo,
                                          -id,
                                          -code,
                                          -HDI_value,
                                          -HDI_rank)

asie_musique <- filter(music_spotify, Continent == 'Asie')
afrique_musique <- filter(music_spotify, Continent == 'Afrique')
oceanie_musique <- filter(music_spotify, Continent == 'Océanie')
AS_musique <- filter(music_spotify, Continent == 'Amérique du Sud')
AN_musique <- filter(music_spotify, Continent == 'Amérique du Nord')
europe_musique <- filter(music_spotify, Continent == 'Europe')

#les différents continents
valeurs_uniques <- resultat %>% distinct(resultat$Continent)

##################################
# Selection des valeurs numériques
##################################

asie_num <- select(asie_musique, where(is.numeric))
afrique_num <- select(afrique_musique, where(is.numeric))
oeanie_num <- select(oceanie_musique, where(is.numeric))
AS_num <- select(AS_musique, where(is.numeric))
AN_num <- select(AN_musique, where(is.numeric))
europe_num <- select(europe_musique, where(is.numeric))

##################################
# GRAPHIQUE RADAR
##################################

dataRadar_asie <- asie_num
dataRadar_afrique <- afrique_num
dataRadar_oceanie <- oeanie_num
dataRadar_AS <- AS_num
dataRadar_AN <- AN_num
dataRadar_europe <- europe_num


# Initialiser des listes pour stocker les médianes
medianes_liste_asie <- list()
medianes_liste_afrique <- list()
medianes_liste_oceanie <- list()
medianes_liste_AS <- list()
medianes_liste_AN <- list()
medianes_liste_europe <- list()

# Boucle pour stocker les médianes de chaque attribut
for (colonne in names(dataRadar_asie)) {
  mediane_colonne1 <- median(dataRadar_asie[[colonne]])
  medianes_liste_asie[[colonne]] <- mediane_colonne1
}

for (colonne in names(dataRadar_afrique)) {
  mediane_colonne2 <- median(dataRadar_afrique[[colonne]])
  medianes_liste_afrique[[colonne]] <- mediane_colonne2
}

for (colonne in names(dataRadar_oceanie)) {
  mediane_colonne3 <- median(dataRadar_oceanie[[colonne]])
  medianes_liste_oceanie[[colonne]] <- mediane_colonne3
}

for (colonne in names(dataRadar_AS)) {
  mediane_colonne4 <- median(dataRadar_AS[[colonne]])
  medianes_liste_AS[[colonne]] <- mediane_colonne4
}

for (colonne in names(dataRadar_AN)) {
  mediane_colonne5 <- median(dataRadar_AN[[colonne]])
  medianes_liste_AN[[colonne]] <- mediane_colonne5
}

for (colonne in names(dataRadar_europe)) {
  mediane_colonne6 <- median(dataRadar_europe[[colonne]])
  medianes_liste_europe[[colonne]] <- mediane_colonne6
}

# transforme en data frame
mon_data_frame_asie <- data.frame(medianes_liste_asie)
mon_data_frame_afrique <- data.frame(medianes_liste_afrique)
mon_data_frame_oceanie <- data.frame(medianes_liste_oceanie)
mon_data_frame_AS <- data.frame(medianes_liste_AS)
mon_data_frame_AN <- data.frame(medianes_liste_AN)
mon_data_frame_europe <- data.frame(medianes_liste_europe)

# création de deux lignes (min et max) pour le graphique en radar
max_min <- matrix(c(1,0), nrow = 2, ncol = ncol(mon_data_frame_asie))
colnames(max_min) <- names(mon_data_frame_asie)
rownames(max_min) <- c("Max", "Min")

# on ajoute les deux lignes dans chaque dataframe
mon_data_frame_asie <- rbind(max_min, mon_data_frame_asie)
mon_data_frame_afrique <- rbind(max_min, mon_data_frame_afrique)
mon_data_frame_oceanie <- rbind(max_min, mon_data_frame_oceanie)
mon_data_frame_AS <- rbind(max_min, mon_data_frame_AS)
mon_data_frame_AN <- rbind(max_min, mon_data_frame_AN)
mon_data_frame_europe <- rbind(max_min, mon_data_frame_europe)


# Créer le graphique en radar
radarchart(mon_data_frame_asie, caxislabels = c(0,0.25,0.5,0.75,1),
           axistype = 1,
           # Personnaliser le polygone
           pcol = 'orange', 
           #pfcol = scales::alpha('red', 0.5), 
           plwd = 2, plty = 1,
           # Personnaliser la grille
           cglcol = 'grey', cglty = 1, cglwd = 0.8,
           # Personnaliser l'axe
           axislabcol = "grey"
)

par(new = TRUE)

radarchart(mon_data_frame_afrique, caxislabels = c(0,0.25,0.5,0.75,1),
           axistype = 1,
           # Personnaliser le polygone
           pcol = 'green', 
           #pfcol = scales::alpha('red', 0.5), 
           plwd = 2, plty = 1,
           # Personnaliser la grille
           cglcol = 'grey', cglty = 1, cglwd = 0.8,
           # Personnaliser l'axe
           axislabcol = "grey"
)

par(new = TRUE)

radarchart(mon_data_frame_oceanie, caxislabels = c(0,0.25,0.5,0.75,1),
           axistype = 1,
           # Personnaliser le polygone
           pcol = 'blue', 
           #pfcol = scales::alpha('red', 0.5), 
           plwd = 2, plty = 1,
           # Personnaliser la grille
           cglcol = 'grey', cglty = 1, cglwd = 0.8,
           # Personnaliser l'axe
           axislabcol = "grey"
)

par(new = TRUE)

radarchart(mon_data_frame_AS, caxislabels = c(0,0.25,0.5,0.75,1),
           axistype = 1,
           # Personnaliser le polygone
           pcol = 'red', 
           #pfcol = scales::alpha('red', 0.5), 
           plwd = 2, plty = 1,
           # Personnaliser la grille
           cglcol = 'grey', cglty = 1, cglwd = 0.8,
           # Personnaliser l'axe
           axislabcol = "grey"
)

par(new = TRUE)

radarchart(mon_data_frame_AN, caxislabels = c(0,0.25,0.5,0.75,1),
           axistype = 1,
           # Personnaliser le polygone
           pcol = 'cyan', 
           #pfcol = scales::alpha('red', 0.5), 
           plwd = 2, plty = 1,
           # Personnaliser la grille
           cglcol = 'grey', cglty = 1, cglwd = 0.8,
           # Personnaliser l'axe
           axislabcol = "grey"
)

par(new = TRUE)

radarchart(mon_data_frame_europe, caxislabels = c(0,0.25,0.5,0.75,1),
           axistype = 1,
           # Personnaliser le polygone
           pcol = 'brown', 
           #pfcol = scales::alpha('red', 0.5), 
           plwd = 2, plty = 1,
           # Personnaliser la grille
           cglcol = 'grey', cglty = 1, cglwd = 0.8,
           # Personnaliser l'axe
           axislabcol = "grey"
)


legend(
  x=1.4, y=1.3, legend = c("Asie", "Océanie", "Afrique", "Europe", "Amérique du Sud", "Amérique du Nord"), horiz = FALSE,
  bty = "n", pch = 20 , col = c(
    'orange','blue', 'green','brown', 'red', 'cyan' ),
  text.col = "black", cex = 1, pt.cex = 1.5
)



