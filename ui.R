dashboardPage(
  skin = "black",
  title = "Arbres à manger",
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    # Titre
    h2("Arbres à manger"),
    # Inputs
    fluidRow(
      column(
        width = 3,

        selectInput(
          inputId = "nom_arbre",
          label   = "Nom arbre",
          choices = c("Tous", as.character(noms_arbres))
        )
      ),
      column(
        width = 3,

        conditionalPanel(
          condition = "input.nom_arbre != 'Tous'",
          selectInput(
            inputId = "genre_arbre",
            label   = "Genre",
            choices = "Tous",
            selected = "Tous"
          )
        )
      ),
      column(
        width = 3,
        conditionalPanel(
          condition = "input.nom_arbre != 'Tous' && input.genre_arbre != 'Tous'",
          selectInput(
            inputId = "espece_arbre",
            label   = "Espèce",
            choices = "Tous",
            selected = "Tous"
          )
        )
      ),
      column(
        width = 3,
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
    ),
    # Carte & infos
    fluidRow(column(width = 8,
                    leafletOutput("carte_arbre")),
             column(
               width = 4,
               box(
                 title = "Arbre",
                 width = NULL,
                 solidHeader = TRUE,
                 background = "green"
               )
             ))
  )
)
