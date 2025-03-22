kwa_raw <- read_xlsx("data/Incubation/KWA.xlsx")
depths <- read_xlsx("data/Incubation/depths.xlsx")
conc_raw <- read_xlsx("data/Incubation/SeqExtractions.xlsx")
wt_raw <- read_xlsx("data/Incubation/drymass.xlsx", sheet = "weight")
kwa_raw <- read_xlsx("data/Incubation/KWA.xlsx")
porosity <- read_xlsx("data/Incubation/drymass.xlsx", sheet = "porosity")
porosity <- porosity %>%
  group_by(Sample) %>%
  summarise(Porosity = mean(Porosity),
            LOI = mean(LOI))

kwa <- kwa_raw %>% 
  pivot_longer(cols =  Al:Zn, names_to = "Element", values_to = "Conc") %>% 
  mutate(Total = (0.05*Conc)/Weight,
         Element = paste("Total_",Element, sep = "")) %>% 
  select(Sample,Element,Total, Depth) %>% 
  pivot_wider(names_from = Element, values_from = Total)


conc_solid <- conc_raw %>%  
  filter(Sample == "blank") %>%                                                      #
  select(Measurement,Time, "Blank" = Conc)%>%                                        # select the blanks and add as new column
  right_join(filter(conc_raw, Sample != "blank"), by = c("Measurement", "Time")) %>% #
  mutate(Measurement = str_replace_all(Measurement, " ", "_")) %>%
  left_join(wt_raw, by = c("Sample","Depth"))%>% 
  left_join(porosity, by = c("Sample")) %>%
  mutate(Treatment = substr(Sample,1,1)) %>% 
  separate(Measurement, into = c("Fraction", "Parameter"), sep = "_", remove = F)%>% 
  mutate(Volume = get_volume(Fraction),
         Cont = ((Conc - Blank) * Volume) / (Weight * (1 - Porosity))) %>%
  select(Treatment,Depth,Sample,Time,Measurement,Cont)%>% 
  pivot_wider(names_from = Measurement, values_from = Cont) %>% 
  filter(!is.na(BD_TP)) %>% 
  left_join(kwa, by = c("Sample", "Depth")) %>% 
  mutate(NaOH_NRP = NaOH_TP-NaOH_SRP,
         Total_NE = Total_P-NaOH_TP-BD_TP-Bipy_TP-H2O_SRP-HCl_SRP,
         Total_TP = Total_P) %>% 
  select(-NaOH_TP, -Total_P) %>% 
  pivot_longer(cols= H2O_SRP:Total_TP, names_to = "Measurement", values_to = "Cont", values_drop_na = F) %>% 
  filter(!is.na(Cont)) %>% 
  left_join(porosity, by = c("Sample")) %>%
  left_join(depths, by = c("Sample", "Depth")) %>%
  separate(Measurement, into = c("Fraction", "Parameter"), sep = "_", remove = F) %>% 
  mutate(mm = get_mm(Parameter),
         Cont = 1000*Cont/mm)
