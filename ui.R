page_navbar(
  title = "Arbres à manger",
  fillable = TRUE,

  bg = "#005B41",
  theme = bs_theme(
    bg = "#FEFEFE",
    fg = "#0F0F0F",
    primary = "#005B41",
    secondary = "#232D3F",
    success = "#008170",
    base_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono")
  ),
  includeCSS("www/css/style.css"),
  # App style

  nav_panel(
    # Titre
    "Carte",
    # Inputs in R/select_tree.R
    select_tree,
    # Carte & infos
    layout_columns(
      col_widths  = c(8, 4),
      heights_equal = "row",
      height = "300px",


# Carte -------------------------------------------------------------------
      card(full_screen = TRUE,
           # min_height = "300px",
           card_body(
             class = "p-0",
             fillable = TRUE,
             leafletOutput(
               outputId = "carte_arbre",
               width = "100%",
               height = "100%"
             )
           )),


# Description -------------------------------------------------------------

      card(
        class = "bg-primary",
        card_header(class = "bg-dark",
                    "A header"),
        card_body(markdown(
          "Some text with a [link](https://github.com)"
        ))
      )

    )
  ),
  nav_panel("Infos",
            # includeHTML("www/rmd/sources.html")
            includeMarkdown("www/rmd/sources.Rmd")
            ),
  footer = "Made with love"

)
