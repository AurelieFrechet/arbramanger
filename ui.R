page_navbar(
  title = "Arbres à manger",
  fillable = TRUE,
  bg = "#005B41",
  theme =bs_theme(
    bg = "#FEFEFE",
    fg = "#005B41",
    primary = "#005B41",
    secondary = "#232D3F",
    success = "#008170",
    base_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono")
  ),
  includeCSS("www/css/style.css"), # App style

  nav_panel(
    # Titre
    "Carte",
    # Inputs in R/select_tree.R
    select_tree,
    # Carte & infos
    layout_columns(
      id = "carte_container",
      col_widths  = c(8, 4),
      leafletOutput("carte_arbre",
                    width = "100%",
                    height = "100%"),
      card(
        class = "bg-primary",
        card_header(
          class = "bg-dark",
          "A header"
        ),
        card_body(
          markdown("Some text with a [link](https://github.com)")
        )
      )

    )
  ),
  nav_panel("Infos"),
  footer = "Made with love"

)
