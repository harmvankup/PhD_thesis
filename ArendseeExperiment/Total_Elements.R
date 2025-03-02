library(readxl)
library(tidyverse)
library(ggplot2)

# First import all raw data. The concentrations of P, Fe and Mn in the extraction solutions, and the Dryweight data.
wt_2month_raw <- read_xlsx("ArendseeExperiment/20220816_AR_Dryweight.xlsx")
wt_10month_raw <- read_xlsx("ArendseeExperiment/20230321_AR_Dryweight.xlsx")

kwa_2month_raw <- read_xlsx("ArendseeExperiment/KWA.xlsx")
kwa_2month_raw[str_detect(kwa_2month_raw, "<")] <- 0

# Total elements
kwa <- kwa_2month_raw %>% 
  filter(!is.na(depth)) %>% 
  mutate_at(5:21, ~ (.*0.05)/weight) %>% 
  group_by(ID) %>% 
  summarise_all(mean) %>%  mutate(FeP = (Fe/55.845)/(P/30.97),
                                  FeS = (Fe/55.845)/(S/32.06),
                                  netFeP = ((Fe/55.845)- (S/32.06))/(P/30.97),
                                  wtpercViv = ((P/30.97)*0.5*501.49*0.001) ) 
kwabefore <- kwa_2month_raw %>% 
  filter(ID == "before") %>% 
  mutate_at(5:21, ~ (.*0.05)/weight) %>% 
  select( c(ID, depth, Al, Ca, Fe, K, Mg, Mn, Na, P, Pb, S, Ti)) %>%  
  mutate(FeP = (Fe/55.845)/(P/30.97),
                FeS = (Fe/55.845)/(S/32.06),
                netFeP = ((Fe/55.845)- (S/32.06))/(P/30.97))

kwalong <- select(kwa, c(ID, depth, Al, Ca, Fe, K, Mg, Mn, Na, P, Pb, S, Ti)) %>% 
  pivot_longer(cols = 3:last_col(), names_to = "Parameter", values_to = "Value")

kwaplots <- ggplot(  kwalong, 
                     mapping = aes(
                       y = Value,
                       x = depth,
                       color = Parameter)
) +
  
  geom_line()  +
  geom_point(size = 2) +
  facet_grid(cols = vars(Parameter), scales = "free") +
  geom_point( data = pivot_longer(kwabefore, cols = 3:last_col(), names_to = "Parameter", values_to = "Value") , aes( y = Value, x = 0),color="black")+
  
  xlab("Depth (cm)") +
  scale_x_reverse() +
  coord_flip() +
  labs(  y = expression(paste("concentration mg/g")), 
         x = "Depth in cm",
         title = "element profiles" ) +
  theme_bw() +
  theme(      aspect.ratio = 2,
              axis.title=element_text(size=8),
              plot.title = element_text(size=20, face="bold", hjust = 0.5) ,
              axis.text=element_text(size=8,angle = 0, hjust = 0.5),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 8),
              legend.key.size = unit(1.5, "cm"))

show(kwaplots)

ggsave( "kwaplot.png", plot = kwaplots, width = 20, height = 5)
