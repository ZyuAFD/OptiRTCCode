
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, 
                lubridate, 
                magrittr,
                ggplot2,
                scales,
                zoo,
                tidyverse,
                knitr,
              RcppRoll)


#  Plot Theme ------------------
Plot_theme=theme_bw()+
  theme(axis.text=element_text(size=12, 
                               color="grey12"),
        axis.title=element_text(size=14),
        plot.title=element_text(size=18,
                                face="bold"),
        legend.position="bottom",
        legend.title=element_text(size=14),
        legend.text=element_text(size=14, 
                                 color="grey12"),
        strip.text.y = element_text(size = 12),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1))
