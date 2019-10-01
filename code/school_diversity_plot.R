library(data.table)
library(ggplot2)

school <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-24/school_diversity.csv")

str(school)

school <- janitor::clean_names(school)

school[, lapply(.SD, function(x) sum(is.na(x)))]

school[, unique(leaid)]

rowSums(school[, c(6:10)])

school2 <- dcast(school, lea_name ~ school_year, fun = mean, 
                 value.var = c("white", "black", "hispanic", "total"))
school2 <- school2[!is.na(`white_1994-1995`) & !is.na(`white_2016-2017`)]

school2[school, `:=`(st = i.st, d_locale_txt = i.d_locale_txt, diverse = i.diverse), on = "lea_name"]

school2[, lapply(.SD, function(x) sum(is.na(x)))]

ggplot(school2[`total_1994-1995` >= 1e5 & `total_2016-2017` >= 1e5]) + 
  # ggalt::geom_dumbbell(aes(x = `white_1994-1995`, xend = `white_2016-2017`, y = lea_name), 
  #                      size = 1, color = "#268785") + 
  # ggalt::geom_dumbbell(aes(x = `black_1994-1995`, xend = `black_2016-2017`, y = lea_name), 
  #                      size = 1, color = "#985F2A") + 
  # ggalt::geom_dumbbell(aes(x = `hispanic_1994-1995`, xend = `hispanic_2016-2017`, y = lea_name), 
  #                      size = 1, color = "#8A6BBE") + 
  geom_curve(aes(x = `white_1994-1995`, xend = `white_2016-2017`, 
                   y = lea_name, yend = lea_name), arrow = arrow(angle = 5, type = "closed"), color = "#268785", angle = 20) + 
  geom_segment(aes(x = `black_1994-1995`, xend = `black_2016-2017`, 
                   y = lea_name, yend = lea_name), arrow = arrow(angle = 5, type = "closed"), color = "#985F2A") + 
  geom_curve(aes(x = `hispanic_1994-1995`, xend = `hispanic_2016-2017`, 
                   y = lea_name, yend = lea_name), arrow = arrow(angle = 5, type = "closed"), color = "#8A6BBE", angle = 20) + 
  geom_point(aes(`white_1994-1995`, lea_name, size = `total_1994-1995` * `white_1994-1995`, color = "White")) + 
  geom_point(aes(`white_2016-2017`, lea_name, size = `total_2016-2017` * `white_2016-2017`, color = "White")) + 
  geom_point(aes(`black_1994-1995`, lea_name, size = `total_1994-1995` * `black_1994-1995`, color = "Black")) + 
  geom_point(aes(`black_2016-2017`, lea_name, size = `total_2016-2017` * `black_2016-2017`, color = "Black")) + 
  geom_point(aes(`hispanic_1994-1995`, lea_name, size = `total_1994-1995` * `hispanic_1994-1995`, color = "Hispanic")) + 
  geom_point(aes(`hispanic_2016-2017`, lea_name, size = `total_2016-2017` * `hispanic_2016-2017`, color = "Hispanic")) + 
  scale_x_continuous(breaks = c(0, 20, 40, 60, 80), labels = scales::percent(c(0, 20, 40, 60, 80), scale = 1))+ 
  scale_color_manual(name = "Ethnic", values = c("White" = "#268785", "Black" = "#985F2A", "Hispanic" = "#8A6BBE")) + 
  scale_size(breaks = c(1e7, 2e7, 3e7, 4e7), labels = c("100K", "200K", "300K", "400K")) +
  hrbrthemes::theme_ipsum_rc() + 
  labs(size = "Student Count", x = "", y = "", 
       title = "Ethnic Diversity of Schools Whose Students Count over 100 Thousand", 
       subtitle = "Most of these large schools are diserve both in 1994-1995 and 2016-2017 except Los Angeles Unified.", 
       caption = "Data from NCES")

ggsave("../plot/school_diversity_plot.png", width = 14, height = 10)
  