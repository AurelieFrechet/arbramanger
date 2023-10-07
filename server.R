function(input, output, session){


# Changement dans la selection du type d'arbre ----------------------------

  ## Nom arbre ----
  observeEvent(input$nom_arbre, {
    updateSelectInput(session = session,
                      inputId = "genre_arbre",
                      label = "Genre",
                      choices = c("Tous", get_genres(input$nom_arbre)),
                      selected = "Tous")

    updateSelectInput(session = session,
                      inputId = "espece_arbre",
                      label   = "Espèce",
                      choices = "Tous",
                      selected = "Tous")

    updateSelectInput(session = session,
                      inputId = "variete_arbre",
                      label   = "Variété",
                      choices = "Tous",
                      selected = "Tous")
  })

  ## Genre arbre ----
  observeEvent(input$genre_arbre, {
    updateSelectInput(session = session,
                      inputId = "espece_arbre",
                      label   = "Espèce",
                      choices = c("Tous", get_especes(input$nom_arbre, input$genre_arbre)),
                      selected = "Tous")

    updateSelectInput(session = session,
                      inputId = "variete_arbre",
                      label   = "Variété",
                      choices = "Tous",
                      selected = "Tous")
  })

  ## Espece arbre ----
  observeEvent(input$espece_arbre, {
    updateSelectInput(session = session,
                      inputId = "variete_arbre",
                      label   = "Variété",
                      choices = c("Tous", get_varietes(input$nom_arbre, input$genre_arbre, input$espece_arbre)),
                      selected = "Tous")
  })



}
