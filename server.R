function(input, output, session){
  # bs_themer()

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


# Arbres réactifs aux inputs ----------------------------------------------

  df_arbres <- reactive({
    df <- data_arbres
    if(input$nom_arbre[1] != "Tous"){
      df <- df %>%  dplyr::filter(nom_commun %in% input$nom_arbre)

      if(input$genre_arbre[1] != "Tous") {
        df <- df %>%  dplyr::filter(genre %in% input$genre_arbre)

        if(input$espece_arbre[1] != "Tous") {
          df <- df %>%  dplyr::filter(espece %in% input$espece_arbre)
        }

        if(input$variete_arbre != "Tous") {
          df <- df %>%  dplyr::filter(variete %in% input$variete_arbre)
        }
      }
    }

    df
  })

# Carte -------------------------------------------------------------------

output$carte_arbre <- renderLeaflet({
  leaflet(data = df_arbres()) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addCircleMarkers(
      lng = ~ lng,
      lat = ~ lat,
      color = "#005B41",
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
      ),
      layerId = ~gml_id
    )
})


# Infos sur l'arbre -------------------------------------------------------
selected_arbre <- reactive({
  if (!is.null(input$carte_arbre_marker_click)) {
    res <- df_arbres() %>%
      dplyr::filter(gml_id == input$carte_arbre_marker_click$id)

    if (nrow(res) == 0) {
      return(NULL)
    } else {
      return(res)
    }
  } else {
    return(NULL)
  }

})

output$info_header <- renderText({
  if (is.null(selected_arbre())) {
    "Infos"
  } else {
    as.character(selected_arbre()$nom_commun)
  }
})


output$infos <- renderUI({
  if (is.null(selected_arbre())) {
    p("Cliquez sur un arbre pour avoir ses informations")
  } else {
    div(layout_column_wrap(width = 1 / 2,
                           HTML(
                             paste0("<p><i>", selected_arbre()$nom_taxon, "</i></p>")
                           ),
                           HTML(
                             paste0(
                               "<p><i>Age de l'arbre : ",
                               lubridate::year(Sys.Date()) - selected_arbre()$date_plant,
                               "</i></p>"
                             )
                           )))
  }
})

}
