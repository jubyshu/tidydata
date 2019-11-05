library(data.table)
library(ggplot2)
library(leaflet)

squirrels <- fread("~/downloads/nyc_squirrels.csv")

squirrels[, action := lest::case_when(
  running == TRUE ~ "running",
  chasing == TRUE ~ "chasing",
  climbing == TRUE ~ "climbing",
  eating == TRUE ~ "eating",
  foraging == TRUE ~ "foraging", 
  TRUE ~ "nothing"
)]

pal <- colorFactor(c("red", "blue", "green", "yellow", "purple", "orange"), 
                   domain = unique(squirrels$action))

leaflet(squirrels) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addCircles(~long, ~lat, stroke = FALSE, color = ~pal(action)) %>% 
  addLegend(pal = pal, values = ~action, position = "bottomleft")
