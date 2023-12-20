setwd("C:/Users/user/Documents/GitHub/SpotifyTopSongsGeoStat")

library(ggplot2)
#library(ggmap)
library(leaflet)
library(dplyr)

# Charger les données sur l'indice de développement humain des pays et table des codes iso des pays
isoCodes <- read.csv('data/codeISOpays.csv', sep=';')
hdiCountries <- read.csv('data/HDI_data/HDR21-22_Statistical_Annex_HDI.csv', sep=';')
spotifydb <- read.csv('data/universal_top_spotify_songs/universal_top_spotify_songs_Oct_18_28.csv')

#Calculer la moyenne des tempo par pays
data_spotify_summary <- spotifydb %>%
  group_by(country) %>%
  summarise(
    moyenne_tempo = mean(tempo, na.rm = TRUE),
    median_tempo  = median(tempo, na.rm = TRUE)
  )

# Fusionner les données de spotify et codeIso 
spotifyIso <- merge(data_spotify_summary, isoCodes, by.x ="country", by.y ="alpha2", all = TRUE)

#Resultat de la jointure entre la table spotify et la table des codes ISO
data_spotify_Iso <- spotifyIso %>% filter(!is.na(moyenne_tempo) & moyenne_tempo != "")

# Fusionner les données de spotify join à code ISO et les données sur l'IDH des pays
spotifyIsoHDI <- merge(data_spotify_Iso, hdiCountries, by.x = "noms_en_gb", by.y = "Country", all = TRUE)

#Resultat de la jointure entre la table data_spotify_Iso et la table des HDI par pays
data_spotify_Iso_HDI <- spotifyIsoHDI %>% filter(!is.na(moyenne_tempo) & moyenne_tempo != "")

# Sauvegarder le dataframe en tant que fichier CSV
#write.csv(data_spotify_Iso_HDI, file = "spotify_ISO_IDH_Oct_carto.csv", row.names = FALSE)

# Enlever les enregistrements n'ayant pas de country et idh
filtered_data <- data_spotify_Iso_HDI %>% filter(!is.na(HDI_value) | !HDI_value == "")

pays <- st_read("data/world-administrative-boundaries/world-administrative-boundaries.shp")

# Effectuer la jointure entre le shapefile et la table des tops chansons
songs_pays <- merge(pays, filtered_data, by.x = "iso3", by.y = "alpha3", all.x = TRUE)

#C'est le jeu de données qui nous intéresse avec les 72 pays leur idh, le tempo moyen par pays et sa géométrie
songs_pays_spotify <- songs_pays %>% 
  filter(!is.na(moyenne_tempo))

# Créer une palette de couleurs pour l'IDH
palette_couleurs <- colorNumeric(palette = "viridis", domain = songs_pays_spotify$HDI_value)

# Créer la carte Leaflet
carte <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% # Style de la carte
  addPolygons(data = songs_pays_spotify, 
              fillColor = ~palette_couleurs(HDI_value),
              fillOpacity = 0.7,
              color = "white",
              stroke = TRUE,
              weight = 1,
              label = ~noms_en_gb, # Étiquette des pays
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
              ),
              popup = ~paste("Pays: ", noms_en_gb, "<br>IDH: ", HDI_value,  "<br>Tempo moyen: ", moyenne_tempo,  "<br>Tempo median: ", median_tempo )
  )

# Afficher la carte
carte

# Calculer la régression linéaire
modele <- lm(moyenne_tempo ~ HDI_value, data=songs_pays_spotify)

# Nuage de points avec la ligne de régression
plot(songs_pays_spotify$HDI_value, songs_pays_spotify$moyenne_tempo, 
     xlab = "IDH", ylab = "Tempo",
     main = "Relation entre l'IDH et le tempo")
abline(modele, col = "red")  # Ajouter la ligne de régression
