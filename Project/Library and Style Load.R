
if (!require("pacman")) install.packages("pacman")
pacman::p_load(data.table, 
                lubridate, 
                dplyr,
                magrittr,
                tidyr,
                ggplot2,
                scales,
                zoo,
                Coalesce,
                knitr)


#  Plot Theme ------------------
Plot_theme=theme_bw()+
  theme(axis.text=element_text(size=14, 
                               color="grey12", 
                               face="bold"),
        axis.title=element_text(size=18,
                                face="bold"),
        plot.title=element_text(size=18,
                                face="bold"),
        legend.position="bottom",
        legend.title=element_text(size=14),
        legend.text=element_text(size=14, 
                                 color="grey12", 
                                 face="bold"),
        strip.text.y = element_text(size = 12,
                                    face='bold'),
        strip.text.x = element_text(size = 12,
                                    face='bold'),
        axis.text.x = element_text(angle = 45, hjust = 1))
