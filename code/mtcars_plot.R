library(data.table)
library(ggplot2)
library(rvest)

cars <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-15/big_epa_cars.csv")

max_co2 <- cars[, .(co2TailpipeGpm = max(co2TailpipeGpm)), by = .(year)
                ][cars, `:=`(make = i.make, model = i.model), on = .(year, co2TailpipeGpm)]

min_co2 <- cars[, .(co2TailpipeGpm = min(co2TailpipeGpm)), by = .(year)
                ][cars, `:=`(make = i.make, model = i.model), on = .(year, co2TailpipeGpm)]

# cars logo
url <- "https://www.carlogos.org/"

car_name <- read_html(url) %>% 
  html_nodes(".logo1 dd") %>% 
  html_text(trim = TRUE)

car_logo <- read_html(url) %>% 
  html_nodes(".logo1 img") %>% 
  html_attr("src")

logos <- data.table(make = car_name, logo = car_logo)
logos[make == "Mini", make := "MINI"]

logos <- rbind(data.table(make = c("Vector", "Geo", "Plymouth"), 
                    logo = c("https://www.carlogos.org/logo/Vector-Motors-logo.png", 
                             "https://www.carlogos.org/logo/Geo-logo.png", 
                             "https://www.carlogos.org/logo/Plymouth-logo.png")),
               logos)

max_co2[logos, logo := i.logo, on = "make"]
min_co2[logos, logo := i.logo, on = "make"]

ggplot(cars, aes(year, co2TailpipeGpm, group = year)) + 
  geom_violin(aes(fill = year), color = "#994639") + 
  # geom_point(data = max_co2) + 
  # geom_point(data = min_co2) + 
  ggimage::geom_image(aes(image = logo), data = max_co2, size = 0.03) + 
  ggimage::geom_image(aes(image = logo), data = min_co2, size = 0.03) + 
  viridis::scale_fill_viridis() + 
  scale_x_continuous(breaks = seq(1984, 2020, 3), labels = seq(1984, 2020, 3)) + 
  hrbrthemes::theme_ft_rc(grid = "Y") + 
  theme(axis.text = element_text(color = "#eeeeee"), 
        axis.title = element_text(color = "#eeeeee"), 
        legend.position = "none") + 
  labs(title = "Cars' CO2 Emissions, 1984 - 2020", 
       caption = "Data: EPA")
  


