#Chemin absolus d'accès au projet Github
cheminAccesProjetGithub = "E:/COURS ING3/Projet Geostat/" #a modifier
setwd(cheminAccesProjetGithub)

#installation des librairies utilisées dans le script
#install.packages("ggplot2")
#install.packages("ggmap")
#install.packages("dplyr")

#librairies utilisées
library(ggplot2)
library(ggmap)
library(dplyr)

# Charger le fichier des caractéristiques audios des chansons de la bd FMA
echonest <- read.csv('echonest.csv', sep=';')

# Créer un fond de carte du monde
world_map <- map_data("world")

p <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  theme_void()

# Ajouter les localisations des artistes
p <- p + geom_point(data = echonest, aes(x = artist_longitude, y = artist_latitude), color = "orange", size = 2)

# Afficher la carte avec les données
print(p)

# Charger les données sur l'indice de développement humain des pays et table des codes iso des pays
isoCodes <- read.csv('codeISOpays.csv', sep=';')
hdiCountries <- read.csv('HDR21-22_Statistical_Annex_HDI.csv', sep=';')
spotifydb <- read.csv('universal_top_spotify_songs.csv')


# Fusionner les données de spotify et codeIso 
spotifyIso <- merge(
  x = spotifydb, 
  y = isoCodes[,c("alpha2", "nom_fr_fr", "noms_en_gb")], 
  by.x ="country", 
  by.y ="alpha2", 
  all.x = TRUE)

#Resultat de la jointure entre la table spotify et la table des codes ISO
data_spotify_Iso <- spotifyIso %>% filter(!is.na(spotify_id) & spotify_id != "")

# Fusionner les données de spotify join à code ISO et les données sur l'IDH des pays
spotifyIsoHDI <- merge(
  x = data_spotify_Iso, 
  y = hdiCountries[,c("Country", "HDI_rank", "Human Development Index")], 
  by.x = "noms_en_gb", 
  by.y = "Country", 
  all.x = TRUE)

#Resultat de la jointure entre la table data_spotify_Iso et la table des HDI par pays
data_spotify_Iso_HDI <- spotifyIsoHDI %>% filter(!is.na(spotify_id) & spotify_id != "")

# Sauvegarder le dataframe en tant que fichier CSV
write.csv(data_spotify_Iso_HDI, file = "spotify_ISO_IDH.csv", row.names = FALSE)

# Filtrer les enregistrements ayant une valeur nulle
filtered_data <- data_spotify_Iso_HDI %>% filter(is.na(HDI_value) | HDI_value == "")

# Regrouper les données filtrées par noms_en_gb et compter les occurrences
grouped_data <- filtered_data %>%
  group_by(noms_en_gb) %>%
  summarise(count = n())

#Le jeu de données qui nous intéresse est "data_spotify_Iso_HDI" puisqu'il contient les informations de la table intiale spotify avec l'idh de chaque pays et son code ISO associé