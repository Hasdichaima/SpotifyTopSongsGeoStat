setwd("C:/Users/user/Documents/GitHub/SpotifyTopSongsGeoStat")

library(ggplot2)
#library(ggmap)
library(dplyr)

# Charger les données sur l'indice de développement humain des pays et table des codes iso des pays
isoCodes <- read.csv('data/codeISOpays.csv', sep=';')
hdiCountries <- read.csv('data/HDI_data/HDR21-22_Statistical_Annex_HDI.csv', sep=';')
spotifydb <- read.csv('data/universal_top_spotify_songs/universal_top_spotify_songs_Oct_18_28.csv')


# Fusionner les données de spotify et codeIso 
spotifyIso <- merge(spotifydb, isoCodes, by.x ="country", by.y ="alpha2", all = TRUE)

#Resultat de la jointure entre la table spotify et la table des codes ISO
data_spotify_Iso <- spotifyIso %>% filter(!is.na(spotify_id) & spotify_id != "")

# Fusionner les données de spotify join à code ISO et les données sur l'IDH des pays
spotifyIsoHDI <- merge(data_spotify_Iso, hdiCountries, by.x = "noms_en_gb", by.y = "Country", all = TRUE)

#Resultat de la jointure entre la table data_spotify_Iso et la table des HDI par pays
data_spotify_Iso_HDI <- spotifyIsoHDI %>% filter(!is.na(spotify_id) & spotify_id != "")

# Sauvegarder le dataframe en tant que fichier CSV
write.csv(data_spotify_Iso_HDI, file = "spotify_ISO_IDH_Oct.csv", row.names = FALSE)

# Enlever les enregistrements n'ayant pas de country et idh
filtered_data <- data_spotify_Iso_HDI %>% filter(!is.na(HDI_value) | !HDI_value == "")


#Calculer la moyenne des tempo par pays
data_spotify_summary <- filtered_data %>%
  group_by(alpha3) %>%
  summarise(
    moyenne_tempo = mean(tempo, na.rm = TRUE)
  )


