library(dplyr)

source("R/paste2.R")

options(digits = 14)

# Chargement et nettoyage -------------------------------------------------

## Arbres ornements ----
arbres_ornement    <- read.csv2("data/arbres-dornement-rennes.csv", encoding = "UTF-8")
str(arbres_ornement)
colnames(arbres_ornement)[1:2] <- c("geo_point", "geo_shape")
arbres_ornement$donnees <- "Arbres d'ornement"

## Arbres alignements ----
arbres_alignement  <- read.csv2("data/arbres-d-alignement-rennes.csv", encoding = "UTF-8")
str(arbres_alignement)
colnames(arbres_alignement)[1:2] <- c("geo_point", "geo_shape")
arbres_alignement$donnees <- "Arbres d'alignement"

## Arbres Rennes metropole ----
arbres_hors_rennes <- read.csv2("data/arbre_hors_rennes.csv", encoding = "UTF-8")
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

data_arbres <- data_arbres %>%
  mutate(
    nom_taxon     = stringr::str_trim(paste2(genre, espece, variete)),
    nom_latin     = stringr::str_trim(paste2(genre, espece)),
    date_plant    = ifelse(date_plant < 1000, NA, date_plant),
    circonference = ifelse(circonference == 0, NA, circonference),
    hauteur       = ifelse(hauteur == 0, NA, hauteur),

  ) %>%
  select(
    gml_id,
    lat,
    lng,
    nom_commun,
    nom_taxon,
    nom_latin,
    genre,
    espece,
    variete,
    donnees,
    date_plant,
    circonference,
    hauteur
  )

str(data_arbres)
summary(data_arbres)
sort(colnames(data_arbres))






# Identification des arbres -----------------------------------------------


# Uniquement les arbres identifiés
## Genre
sort(table(data_arbres$genre), decreasing = T)
sort(unique(data_arbres$genre))
data_arbres$genre_identifie <- ifelse(data_arbres$genre %in% c("TEST", "NON", "", "SAISIE", "AUTRE"), F, T)

## Espece
sort(table(data_arbres$espece), decreasing = T)
sort(unique(data_arbres$espece))
data_arbres$espece_identifie <- ifelse(data_arbres$espece %in% c("RENSEIGNÉ", "", "SAISIE", "AUTRE"), F, T)

data_arbres$identifie <- data_arbres$genre_identifie & data_arbres$espece_identifie
table(data_arbres$identifie, data_arbres$donnees)

## Nom commun
sort(table(data_arbres$nom_commun), decreasing = T)
sort(unique(data_arbres$nom_commun))
data_arbres$nom_identifie <- ifelse(data_arbres$espece %in% c("NON RENSEIGNÉ", "", "X"), F, T)
table(data_arbres$nom_identifie, data_arbres$donnees)

## Date plantation
table(!is.na(data_arbres$date_plant), data_arbres$donnees)


# Export ------------------------------------------------------------------

# tous les arbres
str(data_arbres)
# 187 432 obs
saveRDS(data_arbres, "data/arbres_rennes_metropole.RDS")


# Genre & espece identifié OU nom_commun
arbres_identifies <- data_arbres %>%
  dplyr::filter(identifie | nom_identifie)

str(arbres_identifies)
# 127 386 obs
saveRDS(arbres_identifies, "data/arbres_rennes_identifies.RDS")

# Clés à corriger : ( données aggrégées)
## Id = Genre_espece_variete
## Genre
## Espece
## Variete
## Nom commun

noms_a_corriger <-
  arbres_identifies %>%
  group_by(nom_latin, genre, espece, variete, nom_commun) %>%
  summarise(n = n())

write.csv(noms_a_corriger, "data/Noms_arbres_a_corriger.csv")

rm(list = ls())
