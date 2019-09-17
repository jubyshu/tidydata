library(data.table)
library(ggplot2)
# library(rvest)

# data -> https://github.com/rfordatascience/tidytuesday/tree/master/data/2019/2019-09-17
park_visits <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-17/national_parks.csv")
centroids <- fread("https://opendata.arcgis.com/datasets/c54be84491364a04a0caecc837ab492a_0.csv")

# fill NAs in parkname
park_visits[is.na(parkname), 
            parkname := mapply(function(x, y) gsub(x, "", y), unit_type, unit_name)]

# visitors by park and state
visitors <- park_visits[year != "Total", 
                        .(visitor = sum(visitors, na.rm = TRUE)),
                        by = .(parkname, state, gnis_id)]
visitors[centroids, `:=`(long = i.X, lat = i.Y), on = c(gnis_id = "GNIS_ID")]

state_park <- visitors[, rank := frank(-visitor, ties.method = "dense"), by = state]

# abbr <- read_html("https://www.50states.com/abbreviations.htm") %>% 
#   html_table() %>% 
#   as.data.table()
# setnames(abbr, names(abbr), c("state", "abbr"))
# abbr <- abbr[abbr != "Abbreviation:"]
# state_park[abbr, fullname := i.state, on = c(state = "abbr")]
# state_park[, fullname := tolower(fullname)]

state_park <- state_park[rank == 1 & !state %in% c("PR", "VI", "GU", "AS", "AK","HI")]

state <- map_data("state")
# setDT(state)
# state[state_park, `:=`(park = i.parkname, visitor = i.visitor, 
#                        x = i.long, y = i.lat), on = c(region = "fullname")]

ggplot() +
  geom_polygon(data = state, aes(long, lat, group = group), color = "white", 
               fill = "#f4ead5") + 
  geom_point(data = state_park, aes(long, lat, size = visitor), color = "#3c7f24") + 
  ggrepel::geom_text_repel(data = state_park, aes(long, lat, label = parkname), 
                           color = "#1759a6", size = 3) + 
  scale_size(breaks = c(2e8, 4e8, 6e8, 8e8), 
             labels = c("200m", "400m", "600m", "800m")) + 
  hrbrthemes::theme_ipsum_rc(grid = "none") + 
  theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        legend.position = c(0.1, 0.1)) + 
  labs(title = "The Most Popular National Park in Every States", 
       size = "Visits", 
       subtitle = "Visits cumulate from 1900s")

ggsave("../plot/national_parks.png", width = 12, height = 7.5)
