library(dplyr)
library(leaflet)
options(digits = 14)



# Chargement et nettoyage -------------------------------------------------

data_arbres <- read.csv2("data/arbres-dornement-rennes.csv")

colnames(data_arbres)[1:2] <- c("geo_point", "geo_shape")

data_arbres <- data_arbres %>%
  mutate(
    genre = as.factor(genre),
    espece = as.factor(espece),
    variete = as.factor(variete),
    gestion = as.factor(gestion),
    nom_commun = as.factor(nom_commun),
    date_plant = ifelse(date_plant == 0, NA, date_plant),
    circonference = ifelse(date_plant == 0, NA, circonference),
    hauteur = ifelse(date_plant == 0, NA, hauteur)
  )

divide_geopoint <- data_arbres$geo_point %>% strsplit(',')
data_arbres$lat = sapply(divide_geopoint, '[', 1) %>% as.numeric()
data_arbres$lng = sapply(divide_geopoint, '[', 2) %>% as.numeric()

str(data_arbres)
summary(data_arbres)


# Exploration données manquantes ------------------------------------------
# Pattern ?
na_noms <- data_arbres %>%
  dplyr::filter(nom_commun == "")

summary(na_noms)

carte_nas <-
  leaflet(data = na_noms[sample(1:nrow(na_noms), 200), ]) %>%
  addTiles() %>%
  addMarkers(lng = ~ lng,
             lat = ~ lat)

carte_nas
# Se ne sont pas des arbres spécifiques... Tant pis


# Conserve que les arbres identifiés
data_arbres <- data_arbres %>%
  dplyr::filter(nom_commun != "")

saveRDS(data_arbres, "data/arbres_identifies.RDS")


# Carte -----------------------------------------------------------------------
pal <- colorNumeric(
  palette = "Greens",
  domain = as.integer(data_arbres$nom_commun))


carte_arbre <- leaflet(data = data_arbres) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~ lng,
    lat = ~ lat,
    color = ~pal(as.integer(nom_commun)),
    radius = 5,
    stroke = FALSE,
    opacity = 1,
    fillOpacity = 1,
    popup = ~ paste(
      "Nom commun: ",
      nom_commun,
      "<br> Genre: ",
      genre,
      "<br> Espèce: ",
      espece,
      "<br> Variété: ",
      variete
    )
  )

carte_arbre
