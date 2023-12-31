setwd("C:/Users/user/Documents/GitHub/SpotifyTopSongsGeoStat")
#install.packages("ggplot2")
#install.packages("ggmap")
#install.packages("leaflet")
#install.packages("dplyr")
        
library(ggplot2)
#library(ggmap)
library(leaflet)
library(dplyr)
library(sf)


# Charger les données sur l'indice de développement humain des pays et table des codes iso des pays
isoCodes <- read.csv('data/codeISOpays.csv', sep=';')
hdiCountries <- read.csv('data/HDI_data/HDR21-22_Statistical_Annex_HDI.csv', sep=';')
spotifydb <- read.csv('data/universal_top_spotify_songs/universal_top_spotify_songs_Oct_18_28.csv')

#Calculer la moyenne des tempo par pays
data_spotify_summary <- spotifydb %>%
  group_by(country) %>%
  summarise(
    moyenne_tempo = mean(tempo, na.rm = TRUE),
    median_tempo  = median(tempo, na.rm = TRUE),
    moyenne_danceability = mean(danceability, na.rm = TRUE),
    median_danceability  = median(danceability, na.rm = TRUE)
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
palette_couleurs <- colorNumeric(palette = "Blues", domain = songs_pays$HDI_value)

# Convertir votre dataframe en objet sf
songs_pays_spotify_sf <- st_as_sf(songs_pays)

# Obtenir les centroïdes des ppays
centroids <- st_centroid(songs_pays_spotify_sf)

# Extraire les coordonnées des centroïdes
centroid_coords <- st_coordinates(centroids)

# Definir la taille des centroides
# Transformez les classements en utilisant le logarithme
centroids$size <- log(centroids$moyenne_danceability + 1)  # Ajout de 1 pour éviter le logarithme de 0



################Test 1##################
# Création de la carte Leaflet
carte <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = songs_pays_spotify_sf,
              fillColor = ~palette_couleurs(HDI_value),
              fillOpacity = 0.7,
              color = "white",
              stroke = TRUE,
              weight = 1,
              label = ~noms_en_gb, 
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
              ),
              popup = ~paste("Pays: ", noms_en_gb, "<br>Danceability: ", moyenne_danceability, "<br>IDH: ", HDI_value)
  ) %>%
  addCircleMarkers(data = centroids,
                   radius = ~sqrt(moyenne_danceability) * 5, # Taille proportionnelle à l'IDH
                   color = "red", # Couleur des bulles
                   stroke = FALSE,
                   fillOpacity = 0.7,
                   label = ~noms_en_gb, # Étiquette des bulles
                   popup = ~paste("Pays: ", noms_en_gb, "<br>Danceability: ", moyenne_danceability)
  )  

# Afficher la carte
carte

################Test 2##################
# Créer la carte Leaflet avec les centroïdes pour positionner les bulles
carte <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = songs_pays_spotify, 
              fillColor = ~palette_couleurs(moyenne_danceability),
              fillOpacity = 0.7,
              color = "white",
              stroke = TRUE,
              weight = 1,
              label = ~noms_en_gb,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto"
              ),
              popup = ~paste("Pays: ", noms_en_gb, "<br>IDH: ", HDI_value,  "<br>Tempo moyen: ", moyenne_tempo,  "<br>Tempo median: ", median_tempo )
  ) %>%
  addCircles(data = as.data.frame(centroid_coords), # Utiliser les centroïdes comme données pour les bulles
             lat = ~Y,  # Latitude du centroïde
             lng = ~X,  # Longitude du centroïde
             radius = ~songs_pays_spotify$HDI_value * 100,  
             color = "brown",  
             fillOpacity = 0.7,
             popup = ~paste("Pays: ", songs_pays_spotify$noms_en_gb, "<br>Danceability: ", songs_pays_spotify$moyenne_danceability)
  )


# Afficher la carte
carte

###########Test 3###########
# Créer le graphique ggplot
# Charger un fond de carte
map_world <- map_data("world") # Charger les données mondiales

# Créer le graphique ggplot
graphique <- ggplot() +
  geom_tile(data = map_world, aes(x = long, y = lat, group = group), fill = "lightgrey") + # Fond de carte
  geom_sf(data = songs_pays_spotify_sf, aes(fill = HDI_value), color = "white") +
  scale_fill_gradient(low = "red", high = "green", name = "IDH") +
  labs(title = "Songs danceability by country", caption = "Source: Données Spotify & IDH du UNDP") +
  theme_minimal() +
  geom_point(data = centroids, aes(x = st_coordinates(centroids)[, 1], y = st_coordinates(centroids)[, 2], size = moyenne_danceability), color = "brown", alpha = 0.5) +
  scale_size(name = "Danceabilty", guide = "legend") +
  labs(size = "Taille des bulles", x="Longitude", y="Latitude")

# Afficher le graphique
graphique

# Calculer la corrélation entre l'IDH et la danceability
correlation_idh_danceability <- cor(songs_pays_spotify$HDI_value, songs_pays_spotify$median_danceability)

# Afficher la corrélation
print(correlation_idh_danceability)

#############################

# Calculer la régression linéaire
modele <- lm(moyenne_danceability  ~ HDI_value, data=songs_pays_spotify)
summary(modele)

# Nuage de points avec la ligne de régression
plot(songs_pays_spotify$HDI_value, songs_pays_spotify$moyenne_danceability, 
     xlab = "IDH", ylab = "Danceability",
     main = "Relation entre l'IDH et la Danceabilité d'une chanson")
abline(modele, col = "red")  # Ajouter la ligne de régression


##################Influence de la géolocalisation sur la popularité#####################
#Selectionner la top chanson mondiale
top_chanson_mondiale <- subset(spotifydb, daily_rank == "1" & country =="" & snapshot_date =="2023-10-18")

#Selectionner la même top chanson mondiale pour voire sa popularité par pays
top_chanson_all <- subset(spotifydb, spotify_id == top_chanson_mondiale$spotify_id & snapshot_date == top_chanson_mondiale$snapshot_date)

#Enlever la top mondiale qui a country vide pour n'avoir que les pays où la chanson est un hit
#Resultat de la jointure entre la table spotify et la table des codes ISO
top_chanson_pays <- top_chanson_all %>% filter( country != "")

# Fusionner ces données avec ceux des codeIso 
top_chanson_pays_iso <- merge(top_chanson_pays, isoCodes, by.x ="country", by.y ="alpha2", all = FALSE)

# Fusionner ces données avec les données sur l'IDH des pays
top_chanson_pays_iso_idh <- merge(top_chanson_pays_iso, hdiCountries, by.x = "noms_en_gb", by.y = "Country", all = FALSE)

# Effectuer la jointure entre le shapefile et la table des tops chansons
top_chanson_pays_iso_idh_pays <- merge(pays, top_chanson_pays_iso_idh, by.x = "iso3", by.y = "alpha3", all.x = TRUE)


# Calculer la régression linéaire
modele_top_chanson <- lm(daily_rank  ~ HDI_value, data=top_chanson_pays_iso_idh)
summary(modele_top_chanson)

# Nuage de points avec la ligne de régression
plot(top_chanson_pays_iso_idh$HDI_value, top_chanson_pays_iso_idh$daily_rank, 
     xlab = "IDH", ylab = "Classement chanson",
     main = "Relation entre l'IDH et la popularité d'une chanson")
abline(modele_top_chanson, col = "red")  # Ajouter la ligne de régression

#Afficher ce lien cartographiquement 
#write.csv(top_chanson_pays_iso_idh_pays, file = "C:/Users/hasdi/OneDrive/Desktop/Projet Proba Stat/top_chanson_pays_iso_idh_pays.csv", row.names = FALSE)
my_data <- top_chanson_pays_iso_idh_pays
# Convertir le champ 'geometry' en un objet 'sf'
data_sf <- st_as_sf(my_data, wkt = "geometry")

# Calculer les centroïdes des polygones (centres des pays)
data_centroids <- st_centroid(data_sf)


# Inverser le classement pour une meilleure représentation (les meilleurs classements auront de plus grandes tailles)
data_centroids$normalized_rank <- 1 / my_data$daily_rank

# Transformez les classements en utilisant le logarithme
data_centroids$log_rank <- log(data_centroids$normalized_rank + 1)  # Ajout de 1 pour éviter le logarithme de 0

# Utilisez les résultats transformés comme taille des bulles
data_centroids$size <- data_centroids$log_rank * 5  

# Créer la carte avec les bulles positionnées sur les centroïdes et une échelle logarithmique pour la taille des bulles
ggplot() +
  geom_sf(data = data_sf, aes(fill = HDI_value), color = "white") +
  scale_fill_gradient(low = "red", high = "green", name = "IDH") +
  labs(title = "Popularité de la chanson par pays", caption = "Source: Données Spotify & IDH du UNDP") +
  theme_minimal() +
  geom_point(data = data_centroids, aes(x = st_coordinates(data_centroids)[, 1], y = st_coordinates(data_centroids)[, 2], size = size), color = "black", alpha = 0.5) +
  scale_size(name = "Populrité", guide = "legend") +
  labs(size = "Taille des bulles", x="Longitude", y="Latitude")

# Calculer la corrélation entre l'IDH et la danceability
correlation_idh_popularity <- cor(my_data$HDI_value, my_data$daily_rank, use = "complete.obs")

# Afficher la corrélation
print(correlation_idh_popularity)

# Visuliser les résidus 

# Calculer les résidus
residus <- residuals(modele_top_chanson)

# Joindre les résidus avec les données géographiques des pays
top_chanson_pays_iso_idh$residus <- residus

# Créer la carte avec les résidus positionnés sur les centroïdes des pays
ggplot() +
  geom_sf(data = top_chanson_pays_iso_idh ,aes(fill = HDI_value), color = "white") +
  scale_fill_gradient(low = "red", high = "green", name = "IDH") +
  labs(title = "Résidus de la régression linéaire entre l'IDH et la popularité des chansons", caption = "Source: Données Spotify & IDH du PNUD") +
  theme_minimal() +
  geom_point(data = data_centroids, aes(x = st_coordinates(data_centroids)[, 1], y = st_coordinates(data_centroids)[, 2], size = residus), color = "black", alpha = 0.5) +
  scale_size(name = "Résidus", guide = "legend") +
  labs(size = "Taille des bulles", x="Longitude", y="Latitude")
