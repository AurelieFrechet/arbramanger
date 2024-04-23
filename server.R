function(input, output, session) {
  # bs_themer()

  # Changement dans la selection du type d'arbre ----------------------------

  ## Nom arbre ----
  observeEvent(input$nom_arbre, {
    updateSelectInput(
      session = session,
      inputId = "genre_arbre",
      label = "Genre",
      choices = c("Tous", get_genres(input$nom_arbre)),
      selected = "Tous"
    )

    updateSelectInput(
      session = session,
      inputId = "espece_arbre",
      label   = "Espèce",
      choices = "Tous",
      selected = "Tous"
    )

    updateSelectInput(
      session = session,
      inputId = "variete_arbre",
      label   = "Variété",
      choices = "Tous",
      selected = "Tous"
    )
  })

  ## Genre arbre ----
  observeEvent(input$genre_arbre, {
    updateSelectInput(
      session = session,
      inputId = "espece_arbre",
      label   = "Espèce",
      choices = c("Tous", get_especes(input$nom_arbre, input$genre_arbre)),
      selected = "Tous"
    )

    updateSelectInput(
      session = session,
      inputId = "variete_arbre",
      label   = "Variété",
      choices = "Tous",
      selected = "Tous"
    )

  })

  ## Espece arbre ----
  observeEvent(input$espece_arbre, {
    updateSelectInput(
      session = session,
      inputId = "variete_arbre",
      label   = "Variété",
      choices = c(
        "Tous",
        get_varietes(input$nom_arbre, input$genre_arbre, input$espece_arbre)
      ),
      selected = "Tous"
    )
  })


  # Arbres réactifs aux inputs ----------------------------------------------

  df_arbres <- reactive({
    df <- data_arbres
    if (input$nom_arbre[1] != "Tous") {
      df <- df %>%  dplyr::filter(nom_commun %in% input$nom_arbre)

      if (input$genre_arbre[1] != "Tous") {
        df <- df %>%  dplyr::filter(genre %in% input$genre_arbre)

        if (input$espece_arbre[1] != "Tous") {
          df <- df %>%  dplyr::filter(espece %in% input$espece_arbre)
        }

        if (input$variete_arbre != "Tous") {
          df <- df %>%  dplyr::filter(variete %in% input$variete_arbre)
        }
      }
    }

    df
  })



  # Carte -------------------------------------------------------------------

  output$carte_arbre <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
                                      updateWhenIdle = TRUE)         # map won't load new tiles when panning)
      )  %>%
      setView(lng = -1.6777926,
              lat = 48.117266,
              zoom = 12)
  })

  observe({
    leafletProxy("carte_arbre", session) %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = df_arbres(),
        lng = ~ lng,
        lat = ~ lat,
        color = "#005B41",
        radius = 3,
        stroke = FALSE,
        opacity = 1,
        fillOpacity = 1,
        popup = ~ description_popup(nom_commun,
                                    genre,
                                    espece,
                                    variete),
        layerId = ~ gml_id
      )
  })



  # Infos sur l'arbre -------------------------------------------------------
  selected_arbre <- reactive({
    if (!is.null(input$carte_arbre_marker_click)) {
      res <- df_arbres() %>%
        dplyr::filter(gml_id == input$carte_arbre_marker_click$id)

      ## Selection de l'arbre ----------------------------------------------------
      if (nrow(res) == 0) {
        return(NULL)
      } else {
        return(res)
      }
    } else {
      return(NULL)
    }

  })


  ## Entete ------------------------------------------------------------------
  output$info_header <- renderText({
    if (is.null(selected_arbre())) {
      "Infos"
    } else {
      as.character(selected_arbre()$nom_commun)
    }
  })


  output$infos <- renderUI({
    # Si pas d'arbre sélectionné
    if (is.null(selected_arbre())) {
      return(p("Cliquez sur un arbre pour avoir ses informations"))
    }

    infos <- div(HTML(
      nom_latin(
        selected_arbre()$genre,
        selected_arbre()$espece,
        selected_arbre()$variete
      ),
      "<br>",
      plante_le(selected_arbre()$date_plant)
    ))


    ## Enrichissement Arboreturm --------------------------------------------

  if (!is.na(selected_arbre()$id_svg)) {
      infos <- tagList(
        infos,
         br(),
         fluidRow(column(width = 5,
                         HTML(
                           croquis(id_svg = selected_arbre()$id_svg)
                         )),
                  column(width = 7,
                         HTML(
                           description_beaulieu(id_svg = selected_arbre()$id_svg,
                                                df_beaulieu = data_beaulieue)
                         ))))
    }

    infos

  })



}
