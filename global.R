library(dplyr)
library(leaflet)
library(markdown)
library(bslib)



# Chargement des donnees --------------------------------------------------

data_arbres <- readRDS("data/jointure_arboretum.RDS") %>%
  dplyr::filter(identifie == "Identifié") %>%
  mutate(nom_commun == new_nom_commun)

noms_arbres     <- sort(unique(data_arbres$nom_commun))
genre_arbres    <- sort(unique(data_arbres$genre))
especes_arbres  <- sort(unique(data_arbres$espece))
varietes_arbres <- sort(unique(data_arbres$variete))


data_beaulieue <- readxl::read_excel("data/arboretum/donnees_arboretum.xlsx")

get_genres <- function(nom_arbre) {
  unique(data_arbres$genre[data_arbres$nom_commun == nom_arbre]) %>%
    sort() %>%
    as.character()
}
get_genres("CERISIER")

get_especes <- function(nom_arbre, genre_arbre) {
  unique(data_arbres$espece[data_arbres$nom_commun == nom_arbre
                            & data_arbres$genre == genre_arbre]) %>%
    sort() %>%
    as.character()
}
get_especes("CERISIER", "PRUNUS")

get_varietes  <- function(nom_arbre, genre_arbre, espece_arbre) {
  unique(data_arbres$variete[data_arbres$nom_commun == nom_arbre
                             & data_arbres$genre == genre_arbre
                             & data_arbres$espece == espece_arbre]) %>%
    sort() %>%
    as.character()
}
get_varietes("CERISIER", "PRUNUS", "AVIUM")


