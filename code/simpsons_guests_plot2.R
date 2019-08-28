library(data.table)
library(ggplot2)
library(showtext)

# use google font
font_add_google("Kalam", "Kalam")
showtext_auto()

simpsons <- fread("../data/simpsons_guests.csv", quote = "")

# guest star appearances > 5
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
grad_color <- scales::seq_gradient_pal("#F17C67", "#F7C242")(seq(0, 1, length.out = 31))

# begin to plot
quartz()
ggplot(stars) + 
  geom_segment(aes(x = 31, xend = 31 - (total / 5.8), y = guest_star, yend = guest_star, 
                   color = guest_star), size = 1, alpha = 0.5, lineend = "round") + 
  geom_point(aes(season, guest_star, size = N, color = factor(N))) + 
  scale_color_manual(values = grad_color) + 
  scale_x_continuous(sec.axis = sec_axis(~ (31 - .) * 5.8, name = "guest times"), 
                     breaks = c(1:31), labels = c(1:18, "Film", 19:30)) + 
  hrbrthemes::theme_ft_rc(grid = "X", base_family = "Kalam", 
                          caption_family = "Kalam", subtitle_family = "Kalam") + 
  labs(x = "season", y = "", 
       title = "Guest stars Appearances in The Simpsons", 
       subtitle = "Dots: guest times by season; Segments: total guest times.", 
       caption = "Data Source: Wikipedia") + 
  theme(legend.position = "none")

ggsave("../plot/simpsons_guests.png", width = 12, height = 7.5)
