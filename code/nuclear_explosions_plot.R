library(data.table)
library(lubridate)
library(maps)
library(ggplot2)
library(magrittr)
library(gganimate)

nuclear <- fread("../data/nuclear_explosions.csv")
str(nuclear)

# convert date
nuclear[, date_long := ymd(date_long)]

# world map
world <- map_data("world") %>% setDT()

p <- ggplot() + 
  geom_polygon(aes(long, lat, group = group), fill = "white", color = "grey",
               data = world) + 
  geom_point(aes(longitude, latitude, size = yield_upper, color = type), 
             data = nuclear) + 
  scale_size_continuous(range = c(5, 10)) + 
  hrbrthemes::theme_ipsum_rc() + 
  labs(x = "", y = "", color = "Type", 
       title = "Nuclear Explosions in {frame_time}", 
       caption = "Source: SIPRI | Graphic: Juby") + 
  theme(axis.text = element_blank(), 
        legend.position = "none") + 
  transition_time(year) + 
  shadow_wake(wake_length = 0.05, alpha = 0.8) + 
  # shadow_mark() + 
  ease_aes("elastic-in-out")

animate(p, width = 1920, height = 1080)
anim_save("../plot/nuclear_explosions.gif")
