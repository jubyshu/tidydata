library(ggplot2)
library(data.table)
library(cowplot)

lifts <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")

str(lifts)

lifts[, unique(equipment)]

lifts[, table(event)]

lifts[, date := as.Date(date)
      ][, year := year(date)]

lifts <- lifts[order(year)]

total_kg <- rowSums(lifts[, c("best3squat_kg", "best3bench_kg", "best3deadlift_kg")],
                    na.rm = TRUE)
lifts[, total_kg := total_kg]

lifts <- lifts[!is.na(age) & !is.na(bodyweight_kg) & !place %in% c("DQ", "DD") & event != "SB"]

ggplot(lifts) + 
  geom_jitter(aes(age, total_kg, color = sex)) + 
  facet_wrap(~ event, scales = "free")

new_lifts <- lifts[, rank := frank(total_kg, ties.method = "first"), 
                   by = .(sex, event, weight_class_kg)][rank == 1]

setkey(new_lifts, sex, event)

ggplot(new_lifts) + 
  geom_col(aes(weight_class_kg, total_kg, fill = sex), alpha = 0.5) + 
  geom_point(aes(weight_class_kg, age * 3), shape = 18) + 
  scale_y_continuous(sec.axis = sec_axis(~ . / 3, name = "age")) + 
  facet_grid(event ~ sex, scales = "free", space = "free_y")

# female + bench
p1 <- ggplot(new_lifts[sex == "F" & event == "B"]) + 
  geom_col(aes(weight_class_kg, total_kg), alpha = 0.7, width = 0.7, fill = "#B481BB") + 
  geom_point(aes(weight_class_kg, age), color = "#FFB11B", size = 3) + 
  geom_line(aes(weight_class_kg, age, group = 1), color = "#FFB11B") + 
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "age")) + 
  hrbrthemes::theme_ft_rc() + 
  labs(subtitle = "Female - Bench") + 
  theme(axis.text.x = element_text(angle = -10))
p1
# female + total
p2 <- ggplot(new_lifts[sex == "F" & event == "SBD"]) + 
  geom_col(aes(weight_class_kg, total_kg), alpha = 0.7, width = 0.7, fill = "#B481BB") + 
  geom_point(aes(weight_class_kg, age * 6), color = "#FFB11B", size = 3) + 
  geom_line(aes(weight_class_kg, age * 6, group = 1), color = "#FFB11B") + 
  scale_y_continuous(sec.axis = sec_axis(~ . / 6, name = "age")) + 
  hrbrthemes::theme_ft_rc() + 
  labs(subtitle = "Female - Full Power") + 
  theme(axis.text.x = element_text(angle = -10))
p2
# male + bench
p3 <- ggplot(new_lifts[sex == "M" & event == "B"]) + 
  geom_col(aes(weight_class_kg, total_kg), alpha = 0.7, width = 0.7, fill = "#0D5661") + 
  geom_point(aes(weight_class_kg, age), color = "#005CAF", size = 3) + 
  geom_line(aes(weight_class_kg, age, group = 1), color = "#005CAF") + 
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "age")) + 
  hrbrthemes::theme_ft_rc() + 
  labs(subtitle = "Male - Bench") + 
  theme(axis.text.x = element_text(angle = -10))
p3
# male + total
p4 <- ggplot(new_lifts[sex == "M" & event == "SBD"]) + 
  geom_col(aes(weight_class_kg, total_kg), alpha = 0.7, width = 0.7, fill = "#0D5661") + 
  geom_point(aes(weight_class_kg, age * 6), color = "#005CAF", size = 3) + 
  geom_line(aes(weight_class_kg, age * 6, group = 1), color = "#005CAF") + 
  scale_y_continuous(sec.axis = sec_axis(~ . / 6, name = "age")) + 
  hrbrthemes::theme_ft_rc() + 
  labs(subtitle = "Male - Full Power") + 
  theme(axis.text.x = element_text(angle = -10))
p4

p5 <- plot_grid(p1, p2, p3, p4, nrow = 2)

p6 <- ggplot() + 
  geom_blank() + 
  hrbrthemes::theme_ft_rc() + 
  labs(title = "The Best Lift Record in Every Weight Class", 
       subtitle = "The bars represent lift weight and the dots represent lifters' ages. Data from: OpenPowerlifting.org.")

plot_grid(p6, p5, nrow = 2, rel_heights = c(0.1, 1))