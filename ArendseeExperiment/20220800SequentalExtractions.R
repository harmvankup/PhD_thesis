#  ---- load packages ----
library(readxl)
library(tidyverse)
library(ggplot2)



getwd()

#   ---- Import data ---- 
concdata<-read_xlsx("20220800SequentalExtractions.xlsx")
wtdata<-read_xlsx("20220816_AR_Dryweight.xlsx")

#   ---- Make corrections ----

## filter the values of the blanks
blanks <- filter(concdata, is.na(Depth)) %>% 
  group_by(Fraction, Parameter, Measurement) %>%
  summarise(blank = mean(Conc), .groups = "drop") 

## Define porosity values for all measurements
porosity <- data.frame(
  porosity = rep(c(pull(
    wtdata %>% group_by(Sample) %>% summarise(porosity = mean(Porosity)), porosity),0),each = 2,times = 24)
  )

## Compute percentage dryweight from volume of solution and porosity
conc <- concdata %>% bind_cols(porosity) %>% left_join(blanks, by = c("Fraction","Parameter","Measurement")) %>% 
  mutate(volume = case_when(
    Fraction == "H2O" ~ 0.05,
    Fraction == "Bipy" | Fraction == "BD" ~ 0.1,
    Fraction == "NaOH" ~ 0.075,
    T ~ 0 )) %>%  
  mutate(cont = ((Conc-blank)*volume*0.1)/(Weight*(1-porosity)))

#   ---- Analyze Bipy measurements ----

## Filter and group bipy data and avarage
bipy <- filter(conc, Fraction == "Bipy" ) %>%  
  filter( Parameter == "TP" | Parameter == "SRP" ) %>% 
  remove_missing() %>% 
  mutate(Parameter = case_when(
    Parameter == "TP" & Measurement <= 4 ~ "TPnoacid",
    Parameter == "TP" & Measurement > 4 ~ "TPacidified",
    T ~ "SRP"
    )) %>% 
  group_by(Parameter,  Depth) %>% 
  summarise(stdev = sd(cont), cont = mean(cont) ) 


## Plot the bipy profiles
ggplot( bipy, 
         mapping = aes(
           y = cont,
           x = Depth ,
           color = Parameter,
           shape = factor(Parameter))
) +
  geom_point(size = 2) + 
  geom_line() +
  geom_errorbar(aes(ymin=cont-stdev, ymax=cont+stdev), width=.2,
                position=position_dodge(0.05))+
  xlab("Depth (cm)") +
  scale_x_reverse() +
  coord_flip() +
  labs(  y = expression(paste("concentration (percent dw)")), 
         x = "Depth in cm",
         title = "Bipy profiles" ) +
  theme_bw() +
  theme(axis.title=element_text(size=20),
                   plot.title = element_text(size=20, face="bold", hjust = 0.5) ,#hjust is position (left/right/ middle =0.5)
                   axis.text=element_text(size=14,angle = 0, hjust = 0.5),
                   legend.title = element_text(size = 16),
                   legend.text = element_text(size = 13),
                   legend.key.size = unit(1.8, "cm"))

#   ---- Plot the sequential extractions ----

## Define the P fractions
allfrac <- conc %>% filter(Parameter == "TP" | Parameter == "SRP") %>% 
  mutate(Fraction = case_when(
    Fraction == "Bipy" && Parameter == "TP" && Measurement <= 4 ~ NA_character_,
    Fraction == "Bipy" ~ "BipyTP",
    T ~ paste(Fraction, Parameter)
                              )) %>%  
  remove_missing() %>% 
  group_by(Fraction,  Depth) %>% 
  summarise(stdev = sd(cont), cont = mean(cont) ) %>% ungroup()

## only the TP fractions
tpfrac <- allfrac %>% filter(str_detect(Fraction, "TP"))

## The SRP and the TP stacked
mutfrac <- bind_cols(
  rename(
    filter( allfrac, Fraction == "NaOH TP" | Fraction == "H2O TP" ), TP = cont),
  select( rename(
    filter(
            allfrac, Fraction == "NaOH SRP" | Fraction == "H2O SRP" ), SRP = cont), SRP)
  ) %>%  
  mutate(cont = TP-SRP) %>% 
  select(Fraction,Depth,cont, stdev) %>% 
  bind_rows(filter(allfrac, Fraction != "NaOH TP" & Fraction != "H2O TP")) 

## Lables and colours
legend_labels <- c( 
                    "H2O TP; loosly adsorbed Fe",
                    "Bipyridine; ",
                    "BD; ",
                    "NaOH; TP" )
                      
colors <- list(c("chartreuse", "darkolivegreen3","orange","coral4", "darkorange2","darkgoldenrod1","goldenrod"),
               c("orange","coral4", "darkorange2","darkgoldenrod1","goldenrod"),
               c( "palegreen2", "olivedrab1"),
               c("orange","coral4", "darkorange2","darkgoldenrod1","goldenrod"))

## plot the sequential extractions
ggplot( transform( mutfrac, 
                   Fraction = factor(Fraction, 
                                       levels = c("H2O SRP","H2O TP","BipyTP","BD TP","NaOH SRP","NaOH TP")
                                       )), 
       aes( 
           x=Depth),
       scale_x_discrete(position = 'top')) +
  geom_col(data = transform(tpfrac, 
                             Fraction = factor(Fraction, 
                                               levels = c("H2O TP","BipyTP","BD TP","NaOH TP")
                             )),
    mapping = aes( y = cont, fill = Fraction ),
    stat = "identity", position = position_stack(reverse=TRUE))+
  scale_fill_manual(values = colors[[2]], labels = legend_labels, name = "Extractant") +
  geom_col(
    aes(y = cont, color = Fraction),
    stat = "identity", position = position_stack(reverse=TRUE), fill = "transparent")+
  scale_color_manual(values = colors[[1]], labels = NULL, name = "Extractant") +
 # geom_point(aes(),stat = "identity", position = position_stack(reverse=TRUE),color="black")+
 #  geom_line( aes(),stat = "identity", position = position_stack(reverse=TRUE),color="black")+
  coord_flip()+
  scale_x_reverse() + 
  labs(y = expression(paste("Extracted P (% gDW"^-1,")" )),
       x = "Depth (cm)"
  ) +
  theme_bw()+
  theme(
    axis.title=element_text(size=20),plot.title = element_text(hjust = 0.5),
    axis.text=element_text(size=16,angle = 0, hjust = 0.5),
    legend.title = element_text( size = 20),
    legend.text = element_text(size = 15),
    legend.key.size = unit(1.3, "cm")) 

# ---- Fe/P ratio ----

# ---- PW profiles ----



