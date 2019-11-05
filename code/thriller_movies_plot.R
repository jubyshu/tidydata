library(data.table)
library(lubridate)
library(ggplot2)

movies <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-22/horror_movies.csv")

movies2 <- movies[, `:=`(
  year = mapply(function(x) fifelse(grepl("^[0-9]{4}", x), as.double(x), year(dmy(x))), release_date),
  month = month(dmy(release_date)))]

movies_month <- movies[, 
                       .(score_mean = mean(review_rating, na.rm = TRUE),
                         score_max = max(review_rating, na.rm = TRUE),
                         socre_min = min(review_rating, na.rm = TRUE),
                         count = .N), by = .(month, year)]

ggplot(movies_month) + 
  ggchicklet::geom_chicklet(aes(factor(month), count, fill = factor(year)), width = 0.5) + 
  scale_fill_brewer(palette = "Set3") + 
  scale_x_discrete(breaks = c(1:12, NA), 
                   labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", 
                              "Sep", "Oct", "Nov", "Dec", "Unknown")) + 
  labs(x = "", y = "", fill = "", 
       title = "Thrilling October!", 
       subtitle = "There are more horror/thriller movies released in October than other months.", 
       caption = "Data: IMDb") + 
  guides(fill = guide_legend(nrow = 1)) + 
  hrbrthemes::theme_ft_rc(grid = "Y", base_family = "Special Elite", 
                          subtitle_family = "Special Elite", 
                          caption_family = "Special Elite") + 
  theme(legend.position = "top", 
        axis.text = element_text(color = "white"))

ggsave("tidydata/plot/thriller_movies.png", width = 12, height = 7.5)
