library(dplyr)
library(leaflet)

options(digits = 14)

# Chargement et nettoyage -------------------------------------------------

## Arbres ornements ----
arbres_ornement    <- read.csv2("../arbres-dornement-rennes.csv", encoding = "UTF-8")
str(arbres_ornement)
colnames(arbres_ornement)[1:2] <- c("geo_point", "geo_shape")
arbres_ornement$donnees <- "Arbres d'ornement"

## Arbres alignements ----
arbres_alignement  <- read.csv2("../arbres-d-alignement-rennes.csv", encoding = "UTF-8")
str(arbres_alignement)
colnames(arbres_alignement)[1:2] <- c("geo_point", "geo_shape")
arbres_alignement$donnees <- "Arbres d'alignement"

## Arbres Rennes metropole ----
arbres_hors_rennes <- read.csv2("../arbre_hors_rennes.csv", encoding = "UTF-8")
str(arbres_hors_rennes)
colnames(arbres_hors_rennes)[1:2] <- c("geo_point", "geo_shape")
arbres_hors_rennes$donnees <- "Arbres hors Rennes"

genre_arbres_hr <- arbres_hors_rennes$nom_taxon %>% strsplit(" ")
arbres_hors_rennes$nom_commun = toupper(arbres_hors_rennes$nom_commun)
arbres_hors_rennes$genre   = sapply(genre_arbres_hr, '[', 1) %>% toupper()
arbres_hors_rennes$espece  = sapply(genre_arbres_hr, '[', 2) %>% toupper()
arbres_hors_rennes$variete = sapply(genre_arbres_hr, '[', 3) %>% toupper()

## Jointure des tables ----
data_arbres <- data.table::rbindlist(list(arbres_ornement, arbres_alignement, arbres_hors_rennes), fill = TRUE)

divide_geopoint <- data_arbres$geo_point %>% strsplit(',')
data_arbres$lat = sapply(divide_geopoint, '[', 1) %>% as.numeric()
data_arbres$lng = sapply(divide_geopoint, '[', 2) %>% as.numeric()
data_arbres$identifie <- ifelse(data_arbres$genre %in% c("", "NON"), "Non identifiés", "Identifiés")

data_arbres <- data_arbres %>%
  mutate(
    nom_taxon  = stringr::str_trim(paste(genre, espece, variete)),
    date_plant = ifelse(date_plant == 0, NA, date_plant),
    circonference = ifelse(date_plant == 0, NA, circonference),
    hauteur = ifelse(date_plant == 0, NA, hauteur)
  ) %>%
  select(
    gml_id,
    lat,
    lng,
    nom_commun,
    nom_taxon,
    genre,
    espece,
    variete,
    donnees,
    identifie,
    date_plant,
    circonference,
    hauteur
  )

str(data_arbres)
summary(data_arbres)
sort(colnames(data_arbres))

saveRDS(data_arbres, "data/arbres_rennes_metropole.RDS")


# Exploration données manquantes ------------------------------------------

addmargins(table(data_arbres$donnees,data_arbres$identifie))

# Pattern ?
na_noms <- data_arbres %>%
  dplyr::filter(identifie == "Non identifiés")

summary(na_noms)

carte_nas <-
  leaflet(data = na_noms[sample(1:nrow(na_noms), 200), ]) %>%
  addTiles() %>%
  addMarkers(lng = ~ lng,
             lat = ~ lat)

carte_nas
# Se ne sont pas des arbres spécifiques... Tant pis



# Carte -----------------------------------------------------------------------
data_arbres <- data_arbres %>%
  dplyr::filter(identifie == "Identifiés")

pal <- colorNumeric(
  palette = "Greens",
  domain = as.integer(data_arbres$genre))


carte_arbre <- leaflet(data = data_arbres) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~ lng,
    lat = ~ lat,
    color = "green",
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

