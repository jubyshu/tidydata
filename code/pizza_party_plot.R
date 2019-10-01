library(data.table)
library(ggplot2)
library(RColorBrewer)
# library(leaflet)

pizza_j <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_jared.csv")
pizza_b <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_barstool.csv")
# pizza_d <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_datafiniti.csv")

# ny_pizza <- pizza_b[city == "New York" & review_stats_all_count >= 20]
# 
# leaflet() %>% 
#   addTiles() %>% 
#   addMarkers(ny_pizza$longitude, ny_pizza$latitude, popup = ny_pizza$name)
# 
# top_pizza <- pizza_j[total_votes >= 20]
# 
# ggplot(top_pizza) + 
#   geom_col(aes(place, votes, fill = answer))

same_pizzaj <- pizza_j[place %in% pizza_b$name]
same_pizzab <- pizza_b[name %in% pizza_j$place]

same_pizzaj[answer == "Fair", answer := "Never Again"]
same_pizzaj[, place := reorder(place, total_votes)]

dup_name <- same_pizzab[duplicated(name), name]
for (i in 1:length(dup_name)) {
  same_pizzab[name == dup_name[i], 
              review_stats_all_average_score := mean(review_stats_all_average_score)]
}
same_pizzab <- same_pizzab[!duplicated(name)]

ggplot() + 
  geom_col(aes(place, votes, fill = answer), data = same_pizzaj, width = 0.5) + 
  geom_point(aes(name, review_stats_all_count / 2, size = review_stats_all_average_score), 
             data = same_pizzab, color = "#D0104C") + 
  geom_segment(aes(x = name, xend = name, y = 0, yend = review_stats_all_count / 2), 
               data = same_pizzab, color = "#D0104C", size = 1) + 
  scale_y_continuous(sec.axis = sec_axis(~ . * 2, name = "reveiw count(barstool)")) + 
  scale_fill_manual(values = brewer.pal(5, "Set3")) + 
  hrbrthemes::theme_ft_rc(grid = "X") + 
  labs(x = "", y = "votes(jared)", 
       fill = "likert rating(jared)", 
       size = "average score(barstool)", 
       title = "Overlapping Pizza Restaurants Recorded by Jared & Barstool", 
       subtitle = "Some pizza restaurants have the same name but are not in the same place. In Jared's data, votes are added up. In Barstool's data, count is added up and scores are averaged.", 
       caption = "Source: Jared Lander and Barstool Sports via Tyler Richards.") + 
  coord_flip()
