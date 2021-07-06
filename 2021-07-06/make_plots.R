library(tidyverse)

holidays <- readRDS("2021-07-06/holidays.RDS")


# line plot of dates
year_rng <- max(holidays$year, na.rm = TRUE) - min(holidays$year, na.rm = TRUE)


p <- ggplot(holidays, 
       aes(x = date_parsed, xend = date_parsed)) + 
    geom_segment(aes(y = -0.3, yend = -.5), size = 0.2, alpha = 0.4) + 
    geom_density(aes(y=..scaled..)) + 
    coord_cartesian(ylim=c(0,1), clip="off") + 
    theme_bw() + 
    theme(axis.ticks.y = element_blank(), 
          axis.text.y = element_blank(), 
          plot.margin = unit(c(1,1,4,1), "lines")) + 
    labs(x = "Year", y = NULL, title = "When are Countries born?")

ggsave("2021-07-06/independence_plot.png", p, width = 8, height = 4)
