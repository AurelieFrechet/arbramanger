#' Nom latin
#' @description
#' Convertit les trois variable en un texte HTML au fbon format pour le nom latin :
#' en italique avec le genre en majuscule :
#' *Genre espece variete*
#'
nom_latin <- function(genre,
                      espece,
                      variete) {
  glue::glue(
    "<i>{genre} {espece} {variete} </i>",
    genre      = stringr::str_to_title(genre),
    espece     = stringr::str_to_lower(espece),
    variete    = ifelse(is.na(variete), "", stringr::str_to_lower(variete))
  )
}

#' Planté en
#'
#' @description
#' Texte à afficher en fonction de la date de plantation
#'
plante_le <- function(date_plant){
  if(is.na(date_plant)){
    return("Pas de renseignement sur la date de plantation")
  } else {
    return(paste("Planté en", date_plant))
  }
}


#' Description Pop Up
#'@description Prépare le texte HTML pour le popup sur la carte
description_popup <-
  function(nom_commun,
           genre,
           espece,
           variete) {
    paste(nom_commun,
          "<br>",
          nom_latin(genre, espece,  variete))
  }


#' Croquis
#'
#' @description Télécharge le dessin au format svg et le renvoi au format HTML
#' afin de l'afficher dans l'application
#' @param id_svg identifiant de l'arbre (identique à celui dans la table)
croquis <- function(id_svg) {
  croquis_path <-
    paste0("www/arboretum_sources/", id_svg, ".svg")

  #If croquis not found ?
  paste(readLines(croquis_path,
                  warn = FALSE),
        collapse = " ") %>%
    stringr::str_replace_all(pattern     = "#000000",
                             replacement = "#FFF")
}

#' Description arbre Arboretum
#'
#' @description
#' Transforme ma table de données de Beaulieue (l'Arboretum) en texte HTML
#' afin de le publier dans l'application à son format d'origine
#'
#'
#' @param id_svg : identifiant de l'arbre dans le tableau
#' @param df_beaulieu : table de données
#'
#' @return : le texte HTML quasimentidentique à la disposition du texte
#' dans l'arboretum

description_beaulieu <- function(id_svg, df_beaulieu) {

  df_beaulieu <- as.data.frame(df_beaulieu)

  attributs_arbres <- colnames(df_beaulieu)[-c(1:3)]

  desc_arbres <- lapply(attributs_arbres, function(col_name) {
    content <- df_beaulieu[df_beaulieu$id_svg == id_svg, col_name]

    if (is.na(content)) {return(NULL)}

    glue::glue(
      "<b>{col_name}:</b> : {content}",
      col_name = stringr::str_to_title(col_name),
      content = stringr::str_trim(content)
    )
  })

  # Remove NULL
  desc_arbres[sapply(desc_arbres, is.null)] <- NULL

  return(paste(desc_arbres, collapse = " <br> "))
}

