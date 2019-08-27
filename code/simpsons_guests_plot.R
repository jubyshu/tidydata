library(data.table)
library(ggplot2)
library(showtext)

font_add_google("Kalam", "Kalam")
showtext_auto()

simpsons <- fread("../data/simpsons_guests.csv")

stars <- simpsons[, .N, keyby = .(season, guest_star)
         ][, total := sum(N), by = guest_star
           ][total > 5]

stars[season == "Movie", season := "18.5"][, season := as.double(season)]

stars[, season := lest::case_when(
  season >= 19 ~ season + 1, 
  season == 18.5 ~ 19, 
  TRUE ~ season)]

stars[, guest_star := reorder(factor(guest_star), total)]

# set gradient color
grad_color <- scales::seq_gradient_pal("#F17C67", "#F7C242")(seq(0, 1, length.out = 27))

quartz()
ggplot(stars) + 
  geom_segment(aes(x = 32, xend = 32 - (total / 5), y = guest_star, yend = guest_star, 
                   color = guest_star), size = 1, alpha = 0.5, lineend = "round") + 
  geom_point(aes(season, guest_star, size = N, color = factor(N))) + 
  scale_color_manual(values = grad_color) + 
  scale_x_continuous(sec.axis = sec_axis(~ (32 - .) * 5, name = "guest times"), 
                    breaks = c(1:32), labels = c(1:18, "Film", 19:30, "")) + 
  hrbrthemes::theme_ft_rc(grid = "X", base_family = "Kalam", 
                          caption_family = "Kalam", subtitle_family = "Kalam") + 
  labs(x = "season", y = "", 
       title = "Top 15 Guest Stars in The Simpsons", 
       subtitle = "Dots: guest times by season; Lines: total guest times.", 
       caption = "Data Source: Wikipedia") + 
  theme(legend.position = "none")