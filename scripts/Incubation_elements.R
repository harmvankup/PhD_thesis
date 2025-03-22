source("scripts/Incubation_SeqKWA.R")



totalElements <- c("Total_TP","Total_S", "Total_Ca","Total_Mn", "Total_Fe", "Total_Al","Total_Ti")
Elements <- totalElements %>% str_remove("Total_")


kwaplotdata <- filter(conc_solid, Measurement %in% totalElements, Time == "after")%>% 
  transform( Parameter = factor(Parameter, levels = Elements))

kwabefore <-  filter(conc_solid, 
                     Measurement %in% totalElements, 
                     Time == "before") %>% 
  group_by(Parameter,Sample) %>% 
  summarise(Value= mean(Cont)) %>% 
  transform( Parameter = factor(Parameter, levels = Elements))

before <- filter(conc_solid, Time == "before")%>% 
  group_by(Treatment, Sample, Measurement, Fraction, Parameter) %>% 
  summarise(sd = sd(Cont), Cont = mean(Cont))

calc_loss <- function(cont,hight, porosity, density){
  load <-  (cont*(1-porosity) * density  )
}

endmembers <- before %>% ungroup %>% 
  select(-Sample,-sd) %>% 
  pivot_wider(names_from = Treatment, values_from = Cont)

mixtotalElements <- c("Total_Mn", "Total_Fe", "Total_Al","Total_Ti")
mixElements <- mixtotalElements %>% str_remove("Total_")

mixdata <- conc_solid %>% left_join(endmembers) %>% 
  filter(Fraction == "Total", Time == "after") %>% 
  mutate(Density = (1*Porosity + 1.25*LOI*(1-Porosity) + 2.65*(1-LOI)*(1-Porosity)),
         Density2 = 1/(1*Porosity + (1/1.25)*LOI*(1-Porosity) + (1/2.65)*(1-LOI)*(1-Porosity)),
         rdeep = (Cont-S)/(D-S),
         rshallow = Cont/S,
         diff_deep = calc_loss( Cont-D, Hight, Porosity, Density),
         diff_shallow =  calc_loss( Cont-S, Hight, Porosity, Density)
  ) 

load <-  mixdata %>% 
  filter(Parameter %in% mixElements) %>% 
  group_by(Sample) %>% 
  summarise(rmean= mean(rdeep),
            rdeep_sd= sd(rdeep),
            rshallow_mean = mean(rshallow),
            rshallow_sd = sd(rshallow))%>% 
  right_join(filter(mixdata)) %>% 
  mutate(theor = case_when(
    Treatment == "A" ~ rmean*D + (1-rmean)*S,
    Treatment == "B" ~ D,
    Treatment == "C" ~ S,
    T ~ 0
  )
  ) %>% 
  mutate(meas = (Cont)*(1-Porosity)*Density,
         theo = theor* (1-Porosity)*Density,
         meas2 = (Cont)*(1-Porosity)*Density2,
         theo2 = theor* (1-Porosity)*Density2,
         diff = meas-theo,
         diff2 = meas2 -theo2,
         diffdw = Cont - theor,
         fill = diff >= 0)