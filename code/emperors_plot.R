# source: https://gitlab.codecentric.de/myriam.traub/tidytuesday/

library(data.table)
library(ggplot2)
library(lubridate)

emperors <- fread("../data/emperors.csv")

# parse date
date_cols <- c("birth", "death", "reign_start", "reign_end")
emperors[, (date_cols) := lapply(.SD, as.Date), .SDcols = date_cols]

# emperors born or reign in BC
emperors_born_bc <- c("Augustus", "Tiberius", "Claudius", "Galba")
emperors_reign_bc <- c("Augustus")
emperors[name %in% emperors_born_bc, 
         birth := update(birth, year = -year(birth))
         ][name %in% emperors_reign_bc, 
           reign_start := update(reign_start, year = -year(reign_start))]

# missing data
emperors[is.na(birth)]

# begin to plot
ggplot(emperors) + 
  geom_vline(xintercept = as.Date("0000-01-01"), linetype = "dotted", color = "white") + 
  geom_segment(aes(x = birth, xend = death, y = reorder(name, reign_start), 
                   yend = reorder(name, reign_start), color = dynasty), 
               alpha = 0.8, size = 2.5, lineend = "round") + 
  geom_text(aes(x = death, y = name, label = cause), 
            size = 2.5, alpha = 0.8, hjust = -0.2) + 
  geom_segment(aes(x = reign_start, xend = reign_end, y = name, yend = name), 
               color = "white", size = 1.5, lineend = "round") + 
  geom_point(aes(death, name), shape = 20, color = "black", size = 1) + 
  annotate("rect", xmin = as.Date("0270-01-01"), xmax = as.Date("0405-01-01"), 
           ymin = 43.5, ymax = 44.5, alpha = 0.01, color = "red", 
           size = 0.3, linetype = "dotted") + 
  annotate("text", x = as.Date("0330-01-01"), y = 44, label = "birth missing", 
           size = 2, hjust = -1, color = "red") + 
  annotate("rect", xmin = as.Date("0275-01-01"), xmax = as.Date("0410-01-01"), 
           ymin = 46.5, ymax = 48.5, alpha = 0.01, color = "red", 
           size = 0.3, linetype = "dotted") + 
  annotate("text", x = as.Date("0335-01-01"), y = 47.5, label = "birth missing", 
           size = 2, hjust = -1, color = "red") + 
  annotate("rect", xmin = as.Date("0300-01-01"), xmax = as.Date("0430-01-01"), 
           ymin = 52.5, ymax = 53.5, alpha = 0.01, color = "red", 
           size = 0.3, linetype = "dotted") + 
  annotate("text", x = as.Date("0355-01-01"), y = 53, label = "birth missing", 
           size = 2, hjust = -1, color = "red") + 
  annotate("rect", xmin = as.Date("0345-01-01"), xmax = as.Date("0480-01-01"), 
           ymin = 60.5, ymax = 61.5, alpha = 0.01, color = "red", 
           size = 0.3, linetype = "dotted") + 
  annotate("text", x = as.Date("0405-01-01"), y = 61, label = "birth missing", 
           size = 2, hjust = -1, color = "red") + 
  hrbrthemes::theme_ft_rc(grid = "X") + 
  labs(title = "Roman Emperors", 
       subtitle = "Lifespan (colored bars), duration of reign (white bars), death and their dynasty", 
       x = "", y = "Emperors") + 
  theme(legend.position = c(0.9, 0.2))