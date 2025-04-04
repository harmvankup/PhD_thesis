
library(readxl)
library(tidyverse)
library(ggplot2)

# First import all raw data. The concentrations of P extraction solutions, and the Dryweight data.
conc_raw <- read_xlsx("Aarhus/Profiles.xlsx")
 conc <- conc_raw %>% 
   mutate(Treatment = substr(Core,1,1),
          Conc = case_when(Conc <0 ~ 0,
                           T ~ Conc)) %>% 
   remove_missing() %>% 
   mutate(totsulf = Conc + ((10^(-6.98))/(10^(-pH)))*Conc ) %>% 
   filter(Core!= "ocontrol", Core != "scontrol") %>% 
   group_by(Depth, Treatment, time) %>% 
   summarise(Conc = mean(totsulf),sd = sd(totsulf))
   


profiles <- ggplot(  conc, 
                     mapping = aes(
                       y = Conc,
                       x = Depth,
                       color = Treatment)) +
  #geom_line()  +
  geom_point(size = 2) +
  geom_errorbar(
    aes(ymin = Conc - sd, ymax = Conc + sd),
    position = position_dodge(width = 0.01),
    width = 0.5)+
  xlab("Depth (um)") +
  scale_x_reverse() +
  coord_flip() +
  labs(  y = expression(paste("concentration uM")), 
         x = "Depth in um",
         title = "profiles" ) +
  facet_grid(.~ time)+
  theme_bw() +
  theme(      aspect.ratio = 2,
              axis.title=element_text(size=8),
              plot.title = element_text(size=20, face="bold", hjust = 0.5) ,
              axis.text=element_text(size=8,angle = 0, hjust = 0.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 8),
              legend.key.size = unit(1.5, "cm"))
print(profiles)

show(kwaplots)
