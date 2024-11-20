library(dplyr)

data_arbres <- readRDS("data/arbres_rennes_identifies.RDS")
str(data_arbres)

data_arbres <- data_arbres %>%
  mutate(id = paste(genre, espece, variete, nom_commun, sep = "_"))

# Regroupement par nom taxon ----------------------------------------------

# Clés à corriger : ( données aggrégées)
## Id = Genre_espece_variete
## Genre
## Espece
## Variete
## Nom commun

noms_a_corriger <-
  data_arbres %>%
  mutate(id = paste(genre, espece, variete, nom_commun, sep = "_")) %>%
  group_by(id, genre, espece, variete, nom_simplifie = nom_commun)  %>%
  summarise(n = n())

write.csv(noms_a_corriger, "data/Noms_arbres_a_corriger.csv")


# Import correction noms  -------------------------------------------------
# correspondance nom taxon et nom vernaculaire
varietes_new <- read.csv(
  "data/data_corriges.csv",
  na.strings = c("NA", ""),
  colClasses = c(
    "character",
    "factor",
    "factor",
    "factor",
    "factor",
    "factor",
    "NULL",
    "NULL",
    "NULL",
    "NULL"
  ),
  encoding = "UTF-8"
)
str(varietes_new)
varietes_new <- varietes_new %>%
  mutate(nom_commun = nom_simplifie)  # Aller-retour dans les noms de variables parce que je suis une chips

data_arbre2 <- data_arbres %>%
  select(-c(genre, espece, variete, nom_commun)) %>%
  left_join(varietes_new, by = "id") %>%
  mutate(nom_taxon = paste(genre, espece),
         nom_latin = paste(genre, espece)) %>%
  select(-id)

View(data_arbre2)
length(unique(data_arbres$nom_commun))
length(unique(data_arbre2$nom_commun))
length(unique(data_arbre2$nom_affichage))

saveRDS(data_arbre2, "data/arbres_identifies_simplifies.RDS")


# Donnees arboretum -------------------------------------------------------
# Lecture données
arboretum <-
  readxl::read_excel("data/arboretum/donnees_arboretum.xlsx")
str(arboretum)
# Recheche correspondance
arboretum$nom_latin <- toupper(arboretum$nom_latin)


non_repertories <-
  setdiff(arboretum$nom_latin, unique(data_arbre2$nom_latin))
arboretum[arboretum$nom_latin %in% non_repertories, ]

jointure_arboretum <-
  data_arbre2 %>% left_join(arboretum, by = "nom_latin")

saveRDS(jointure_arboretum, "data/jointure_arboretum.RDS")
