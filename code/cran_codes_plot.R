library(data.table)
library(ggplot2)
library(RColorBrewer)

cran_code <- fread("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")

tidyverse <- c("readr", "tidyr", "stringr", "dplyr", "tibble", "ggplot2", "purrr", "forcats", 
               "tidyverse")

tv <- cran_code[pkg_name %in% tidyverse
                ][, `:=`(total_code = sum(code), total_comment = sum(comment)), by = pkg_name]
tv[, pkg_name := reorder(pkg_name, total_code)]

ggplot(tv) + 
  geom_col(aes(pkg_name, code, fill = language), width = 0.4, color = "#efefef") + 
  geom_line(aes(pkg_name, total_comment * 5, group = 1), color = "#E03C8A", size = 1) + 
  scale_fill_brewer(palette = "Paired") + 
  scale_y_continuous(sec.axis = sec_axis(~ . / 5, name = "comment")) + 
  hrbrthemes::theme_ft_rc(base_family = "Righteous", grid = "Y", 
                          caption_family = "Righteous") + 
  labs(title = "Tidyverse Packages Codes", x = "", 
       caption = "Source: CRAN | Plot: Juby", fill = "") + 
  theme(axis.text = element_text(color = "white"), 
        legend.position = "top") + 
  guides(fill = guide_legend(nrow = 1))

# ggsave("../plot/cran_codes.png", width = 12, height = 7.5)