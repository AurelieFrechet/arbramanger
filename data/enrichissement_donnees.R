library(readODS)
library(dplyr)

data_arbres <- readRDS("data/arbres_rennes_metropole.RDS")
str(data_arbres)

# Regroupement par nom taxon ----------------------------------------------

varietes_arbres <- data_arbres %>%
  dplyr::filter(identifie == "Identifié") %>%
  group_by(nom_taxon, nom_commun) %>%
  summarise(n = n())

write.csv(varietes_arbres, "data/variete_arbres.csv")



# Import correction noms  -------------------------------------------------
# correspondance nom taxon et nom vernaculaire
varietes_new <- read_ods("../VARIETES.ods",row_names = TRUE,as_tibble = FALSE)
str(varietes_new)

data_arbre2 <- data_arbres  %>%
  left_join(varietes_new, by = c("nom_commun","nom_taxon")) %>%
  mutate(nom_vernaculaire = new_nom_commun)

sort(unique(varietes_new$nom_taxon))
sort(unique(data_arbres$nom_taxon))

View(data_arbre2)
length(unique(data_arbre2$nom_commun))
length(unique(data_arbre2$nom_vernaculaire))


saveRDS(data_arbre2,"data/arbres_identifies_simplifies.RDS" )
