times <- read_xlsx("data/Incubation/timeseries.xlsx")
cores <- read_xlsx("data/Incubation/cores.xlsx")
conc_sulfide <- read_xlsx("data/Incubation/Sulfide.xlsx")
conc_sulfate <- read_xlsx("data/Incubation/Sulfate.xlsx") %>% mutate(Conc = 1000*Conc/mmsulfate, clake = 1000*clake/mmsulfate) #calculate from mg/L to umol/L
conc_P <- read_xlsx("data/Incubation/P.xlsx") %>% mutate(Conc = 1000*Conc/mmP) #calculate from mg/L to umol/L

conc_P <- conc_P %>% 
  filter(Sample == "blank") %>% 
  rename(clake = Conc) %>%  
  select(clake, Timestep) %>% 
  right_join(conc_P,by = c("Timestep"))

conc <- bind_rows("Sulfide" = conc_sulfide, "Sulfate" =  conc_sulfate, "SRP" = conc_P, .id = "Parameter") %>%
  left_join(times, by = c("Timestep")) %>% 
  left_join(cores, by = c("Sample")) %>% 
  filter(Sample != "blank") %>% 
  mutate(Treatment = substr(Sample,1,1))
rates <- conc %>% 
  filter(!is.na(Conc)) %>% 
  arrange(Parameter, Sample, Timestep, Conc) %>% 
  group_by(Parameter, Sample) %>% 
  mutate(Volume = hwater*(3^2)*pi,
         Start = case_when(Parameter == "Sulfate" ~ (lag(Volume)-20)*lag(Conc)/1000 + lag(Add)*10007.972/mmsulfate + ((20-lag(Add))*lag(clake))/1000,
                           Parameter == "SRP" ~ (lag(Volume)-20)*lag(Conc)/1000 + lag(clake)*20/1000,
                           T ~  (lag(Volume)-20)*lag(Conc)/1000),
         End = Volume*Conc/1000,
         diff = (End-Start)/((3^2)*pi))%>% 
  filter(!is.na(diff)) %>%  
  mutate(add = cumsum(diff),
         timediff = Time - lag(Time),
         flux = diff/(timediff))  %>% ungroup

ratesmean  <- rates %>%  group_by(Time,Parameter, Treatment)%>% 
  summarise(stock = mean(Add),
            sdload = sd(add, na.rm=T),
            load = mean(add, na.rm = T),
            total = sum(diff, na.rm = T),
            rate = mean(diff, na.rm =T),
            sdConc = sd(Conc, na.rm=T),
            Conc = mean(Conc, na.rm =T)) 

