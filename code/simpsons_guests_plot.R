library(data.table)
library(ggplot2)
library(showtext)
library(rvest)

# use google font
font_add_google("Kalam", "Kalam")
showtext_auto()

simpsons <- fread("../data/simpsons_guests.csv")

# == #tidytuesday data omit season 15 ==
# season 14 & 19 has dirty data
# simpsons[grepl("\\|", guest_star)]

# remove season 14 & 19
# simpsons <- simpsons[!(season == "14" | season == "19")]
# 
# url <- "https://en.wikipedia.org/wiki/List_of_The_Simpsons_guest_stars_(seasons_1%E2%80%9320)"
# 
# data <- url %>% 
#   read_html() %>% 
#   html_table(fill = TRUE)
# 
# tidy_data <- function(n) {
#   season <- data[[2]] %>% 
#     as.data.table() %>% 
#     .[Season == n]
#   
#   setcolorder(season, c(1, 4:6, 2:3))
#   setnames(season, names(season), names(simpsons))
#   
#   season[, episode_title := gsub("\"(.+)\"(.*)", "\\1", episode_title)]
#   
#   simpsons <- rbind(simpsons, season)
#   
#   return(simpsons)
# }
# 
# simpsons <- tidy_data("14")
# simpsons <- tidy_data("15")
# simpsons <- tidy_data("19")
# 
# simpsons[, guest_star := gsub("\\[.+\\]|\\(.+\\)", "", guest_star)]

# fwrite(simpsons, "../data/simpsons_guests.csv")
# =====================================

stars <- simpsons[, .(.N, role = uniqueN(role)), keyby = .(season, guest_star)
         ][, total := sum(N), by = guest_star
           ][total > 5][order(-total)]

stars[season == "Movie", season := "18.5"][, season := as.double(season)]

stars[, season := lest::case_when(
  season >= 19 ~ season + 1, 
  season == 18.5 ~ 19, 
  TRUE ~ season)]

stars[, guest_star := reorder(factor(guest_star), total)]

# set gradient color
grad_color <- scales::seq_gradient_pal("#F17C67", "#F7C242")(seq(0, 1, length.out = 29))

quartz()
ggplot(stars) + 
  geom_segment(aes(x = 31, xend = 31 - (total / 5.5), y = guest_star, yend = guest_star, 
                   color = guest_star), size = 1, alpha = 0.5, lineend = "round") + 
  geom_point(aes(season, guest_star, size = N, color = factor(N))) + 
  scale_color_manual(values = grad_color) + 
  scale_x_continuous(sec.axis = sec_axis(~ (31 - .) * 5.5, name = "guest times"), 
                    breaks = c(1:31), labels = c(1:18, "Film", 19:30)) + 
  hrbrthemes::theme_ft_rc(grid = "X", base_family = "Kalam", 
                          caption_family = "Kalam", subtitle_family = "Kalam") + 
  labs(x = "season", y = "", 
       title = "Top 17 Guest stars in The Simpsons", 
       subtitle = "Dots: guest times by season; Lines: total guest times.", 
       caption = "Data Source: Wikipedia") + 
  theme(legend.position = "none")

ggsave("../plot/simpsons_guests.png", width = 12, height = 7.5)
