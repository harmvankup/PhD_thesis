library(readxl)
library(tidyverse)
library(ggplot2)


#First import all raw data. The concentrations of P extraction solutions, and the Dryweight data.

conc_raw <- read_xlsx("SequentalExtractions.xlsx")

conc_raw <- pivot_longer(conc_raw,5:11, names_to = "Fraction", values_to = "Conc")

wt_raw <- read_xlsx("Dryweight.xlsx")

#Filter the values of the blanks from the concentration data, and take the mean value for each measurement series.

blanks_2month <- filter(conc_raw, is.na(Depth)) %>% 
  group_by(Fraction, Parameter, Measurement) %>%
  summarise(blank = mean(Conc), .groups = "drop")


# Define porosity values for all measurements

porosity <- 
  wt_raw %>% group_by(Depth) %>% summarise(porosity = mean(Porosity))

# Compute percentage dryweight from volume of solution and porosity
conc_10month <- conc_10month_raw %>% 
  left_join(porosity_10month) %>% 
  left_join(blanks_10month, by = c("Fraction","Measurement")) %>% 
  separate(Fraction, c("Fraction", "Parameter")) %>% 
  mutate(volume = case_when(
    Fraction == "H2O" ~ 0.02,
    Fraction == "Bipy" | Fraction == "BD" ~ 0.04,
    Fraction == "NaOH" ~ 0.03,
    T ~ 0 )) %>%  
  add_column(time = "10month")

conc <- 
  full_join(conc_10month,conc_2month) %>% 
  mutate(cont = ((Conc-blank)*volume)/(Weight*(1-porosity)),
         Depth = Depth +3)

# A whole lot of complicated transformations which would've been unnecessary had I structured my data better
allfrac <- conc %>% select(!Nr) %>% filter(Parameter == "TP" | Parameter == "SRP") %>% 
  mutate(species = case_when(
    time == "2month" & Fraction == "Bipy" & Parameter == "TP" & Measurement >= 5 ~ NA_character_,
    Fraction == "Bipy" ~ "Bipy TP",
    T ~ paste(Fraction, Parameter)
  )) %>%  
  remove_missing() %>% 
  full_join(mutate(filter(select(before, !Nr), (Parameter == "TP" | Parameter == "SRP"), Sample == "Before" ), Depth = 30))

mutfrac <- bind_cols(
  rename(
    filter( allfrac, species == "NaOH TP" ), TP = cont),
  select( rename(
    filter( allfrac, species == "NaOH SRP" ), SRP = cont), SRP) 
) %>%  
  mutate(cont = case_when( TP-SRP <= 0 ~ 0,
                           T ~ TP-SRP
  ),
  species = replace(species, species == "NaOH TP", "NaOH NRP")) %>% 
  select(species,Depth,time,cont) %>% 
  bind_rows(select(filter(allfrac, species != "NaOH TP" & species != "H2O SRP"), !stdev)) 

#Lables and colours
legend_labels <- c( "H2O TP",
                    "Bipyridine; ",
                    "BD; ",
                    "NaOH; SRP" ,
                    "NaOH; NRP" ,
                    "Total P")

colors <- list(c("palegreen2" ,  "tomato3", "plum3", "darkgoldenrod1", "tan3", "burlywood2","goldenrod"),
               c("orange","coral4", "darkorange2","darkgoldenrod1","goldenrod"),
               c( "palegreen2" ),
               c("orange","coral4", "darkorange2","darkgoldenrod1","goldenrod"))

# plot the sequential extractions
seqextr <- ggplot( transform( mutfrac, 
                              Fraction = factor(Fraction, 
                                                levels = c("H2O TP","Bipy TP","BD TP","NaOH SRP","NaOH NRP")
                              ),
                              time = factor(time, levels = (c("2month", "10month")))), 
                   aes( 
                     x=Depth),
                   scale_x_discrete(position = 'top')) +
  geom_col(
    mapping = aes( y = cont, fill = Fraction ),
    stat = "identity", position = position_stack(reverse=TRUE))+
  scale_fill_manual(values = colors[[1]], labels = legend_labels, name = "Extracted Fraction") +
  # geom_point(aes(),stat = "identity", position = position_stack(reverse=TRUE),color="black")+
  #  geom_line( aes(),stat = "identity", position = position_stack(reverse=TRUE),color="black")+
  coord_flip()+
  scale_x_reverse() + 
  labs(y = expression(paste("Extracted P (mg gDW"^-1,")" )),
       x = "Depth (cm)"
  ) +
  facet_grid(cols = vars(time)) +
  theme(
    axis.title=element_text(size=20),plot.title = element_text(hjust = 0.5),
    axis.text=element_text(size=16,angle = 0, hjust = 0.5),
    legend.title = element_text( size = 20),
    legend.text = element_text(size = 15),
    legend.key.size = unit(1.3, "cm")) 


