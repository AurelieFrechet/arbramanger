select_tree <- layout_columns(
  fill = FALSE,
  selectInput(
    inputId = "nom_arbre",
    label   = "Nom arbre",
    choices = c("Tous", as.character(noms_arbres))
  ),

  conditionalPanel(
    condition = "input.nom_arbre != 'Tous'",
    selectInput(
      inputId = "genre_arbre",
      label   = "Genre",
      choices = "Tous",
      selected = "Tous"
    )
  ),

  conditionalPanel(
    condition = "input.nom_arbre != 'Tous' && input.genre_arbre != 'Tous'",
    selectInput(
      inputId = "espece_arbre",
      label   = "Espèce",
      choices = "Tous",
      selected = "Tous"
    )
  ),

  conditionalPanel(
    condition = "input.nom_arbre != 'Tous' && input.genre_arbre != 'Tous' && input.espece_arbre != 'Tous'",
    selectInput(
      inputId = "variete_arbre",
      label   = "Variété",
      choices = "Tous",
      selected = "Tous"
    )
  )
)
