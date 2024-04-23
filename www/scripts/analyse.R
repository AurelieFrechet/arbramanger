library(ggplot2)
library(leaflet)

data_arbres <- readRDS("data/arbres_rennes_identifies.RDS")


# Date de Plantation ------------------------------------------------------

sum(is.na(data_arbres$date_plant)) / nrow(data_arbres)
# 76% de données manquantes

data_arbres_p <- data_arbres %>% filter(!is.na(date_plant))

min_graph <- min(data_arbres_p$date_plant)

data_arbres_p %>%
  ggplot(aes(x = date_plant)) +
  geom_bar() +
  labs(title = 'Date de plantation approximative',
       x = NULL,
       y = NULL)

table(data_arbres_p$date_plant)

data_arbres_p <- data_arbres_p %>%
  mutate(date_plant2 = floor(date_plant / 10) * 10)

data_arbres_p %>%
  ggplot(aes(x = date_plant2)) +
  geom_bar() +
  labs(title = 'Date de plantation approximative',
       x = NULL,
       y = NULL)


# Carte

# map <- map_data("france")
# str(map)
# sort(unique(map$region))
# map_35 <- map[map$region=="Ille-et-Vilaine", ]

pal <- colorNumeric(palette = colorRampPalette(colors = c("#232D3F", "#008170"))(length(unique(
  data_arbres_p$date_plant2
))),
domain = data_arbres_p$date_plant2)


carte_arbre <- leaflet(data = data_arbres_p,
                       options = leafletOptions(preferCanvas = TRUE)) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    lng = ~ lng,
    lat = ~ lat,
    color = ~ pal(date_plant2),
    radius = case_when(
      input$map_zoom <= 5 ~ 1,
      input$map_zoom == 6 ~ 2,
      input$map_zoom == 7 ~ 3,
      input$map_zoom == 8 ~ 5,
      input$map_zoom == 9 ~ 7,
      input$map_zoom == 10 ~ 9,
      input$map_zoom > 11 ~ 11
    ),
    stroke = FALSE,
    opacity = 1,
    fillOpacity = 1,
    popup = ~ paste(
      "<h3> ",
      nom_commun,
      "</h3><br>  <i>",
      paste(genre, espece, variete),
      "</i> <br> <br> Planté en ",
      date_plant
    )
  )

carte_arbre
