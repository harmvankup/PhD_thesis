library(readxl)
library(tidyverse)
library(ggplot2)
library(ggh4x)
library(lemon)
library(knitr)
rm(F)
mmP <- 30.97376
mmS <- 32.065
mmFe <- 55.845

# First import all raw data. The concentrations of P extraction solutions, and the Dryweight data.
depths <- read_xlsx("incubationexperiment/depths.xlsx")
conc_raw <- read_xlsx("incubationexperiment/SeqExtractions.xlsx")
wt_raw <- read_xlsx("incubationexperiment/drymass.xlsx", sheet = "weight")
kwa_raw <- read_xlsx("incubationexperiment/KWA.xlsx")
porosity <- read_xlsx("incubationexperiment/drymass.xlsx", sheet = "porosity")
porosity <- porosity %>%
  group_by(Sample) %>%
  summarise(Porosity = mean(Porosity))

porositydepths <-left_join(porosity, depths, by = c("Sample")) %>%  mutate(Treatment = substr(Sample,1,1))


# Function to calculate volume based on fraction
get_volume <- function(fraction) {
  case_when(
    fraction == "H2O"   ~ 0.02,
    fraction %in% c("BD") ~ 0.0400,
    fraction == "NaOH"  ~ 0.0303,
    fraction == "Bipy" ~ 0.04,
    TRUE                ~ 0
  )
}

kwa <- kwa_raw %>% 
        pivot_longer(cols =  Al:Zn, names_to = "Element", values_to = "Conc") %>% 
        mutate(Total = (0.05*Conc)/Weight,
               Element = paste("Total_",Element, sep = "")) %>% 
        select(Sample,Element,Total, Depth) %>% 
        pivot_wider(names_from = Element, values_from = Total)




conc <- conc_raw %>%  
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
         Total_NE = Total_P-NaOH_TP-BD_TP-Bipy_TP-H2O_SRP,
         Total_TP = Total_P) %>% 
  select(-NaOH_TP, -Total_P) %>% 
  pivot_longer(cols= H2O_SRP:Total_TP, names_to = "Measurement", values_to = "Cont", values_drop_na = F) %>% 
  filter(!is.na(Cont)) %>% 
  separate(Measurement, into = c("Fraction", "Parameter"), sep = "_", remove = F)



### Legend and Labels
legend_labels <- c("Vivianite+OM", "control vivianite", "control OM")
legend_labels_Bipy <- c("H2O","Bipy", "BD", "NaOH SRP", "NaOH NRP", "Total P")
legend_labels_Fe <- c("Bipy", "BD", "Total element")
colors_Bipy <- list(c("palegreen2","red", "darkorange2", "cadetblue","coral4", "grey34"))
colors_Bipy2 <- list(c("red", "darkorange2", "grey31"))
legend_names <- list(
  "A" = "Vivianite+OM" ,
  "B" = "control vivianite",
  "C" = "control OM"
)

variable_labeller <- function(variable,value){
  return(legend_names[value])
}
  
plotdata <- transform( filter(conc, Measurement %in% c("H2O_SRP","Bipy_TP","BD_TP","NaOH_SRP","NaOH_NRP", "Total_NE"), Time != "before" , Sample != "C3"), 
           Measurement = factor(Measurement, 
                             levels = c("H2O_SRP","Bipy_TP","BD_TP","NaOH_SRP","NaOH_NRP", "Total_NE")),
           Treatment = factor(Treatment, levels = (c("A", "B","C"))))

seq_plot <- ggplot( plotdata, 
                      aes( x=Depth, y = Cont, fill = Measurement),
                      scale_x_discrete(position = 'top')) +
  geom_bar(
    mapping = aes( y = Cont, fill = Measurement  ),
    stat = "identity", position = position_stack(reverse=TRUE))+
  scale_fill_manual(values = unlist(colors_Bipy), labels = legend_labels_Bipy, name = "Extracted Fraction") +
  coord_flip()+
  scale_x_reverse() + 
  labs(y = expression(paste("Extracted P (mg gDW"^-1,")" )),
       x = "Depth (cm)"
  ) +
  facet_grid(~ Treatment   ,labeller= variable_labeller) +
  theme_classic()+
  theme(
    axis.title=element_text(size=20),plot.title = element_text(hjust = 0.5),
    axis.text=element_text(size=14,angle = 0, hjust = 0.5),
    legend.title = element_text( size = 20),
    panel.background = element_rect(colour = "grey20",linewidth = 1),
    strip.text = element_text(size=13, face="bold", hjust = 0.5) ,
    strip.background = element_blank(),
    legend.text = element_text(size = 15),
    legend.key.size = unit(1.2, "cm"),
    legend.background = element_rect(colour='grey')) 
show(seq_plot)



seq_plotrep <- reposition_legend(seq_plot, 'bottom right', offset=0.08, panel = 'panel-1-3')
ggsave("SeqExtr_Incubation.png", plot = seq_plotrep, width = 10, height = 7)


before <- filter(conc, Time == "before")%>% 
  group_by(Treatment, Sample, Measurement, Fraction, Parameter) %>% 
  summarise(sd = sd(Cont), Cont = mean(Cont))



beforeplotdata <- transform( filter(before, Measurement %in% c("H2O_SRP","Bipy_TP","BD_TP","NaOH_SRP","NaOH_NRP", "Total_NE")), 
                       Measurement = factor(Measurement, 
                                            levels = c("H2O_SRP","Bipy_TP","BD_TP","NaOH_SRP","NaOH_NRP", "Total_NE")),
                       Treatment = factor(Treatment, levels = (c("D", "S"))))

beforeplotdata$y_pos = NA
beforeplotdata$y_pos[beforeplotdata$Measurement == "H2O_SRP"] = beforeplotdata$Cont[beforeplotdata$Measurement == "H2O_SRP"]

beforeplotdata$y_pos[beforeplotdata$Measurement == "Bipy_TP"] = beforeplotdata$Cont[beforeplotdata$Measurement == "H2O_SRP"] + 
  beforeplotdata$Cont[beforeplotdata$Measurement == "Bipy_TP"]

beforeplotdata$y_pos[beforeplotdata$Measurement == "BD_TP"] = beforeplotdata$y_pos[beforeplotdata$Measurement == "Bipy_TP"] + 
  beforeplotdata$Cont[beforeplotdata$Measurement == "BD_TP"]
beforeplotdata$y_pos[beforeplotdata$Measurement == "NaOH_SRP"] = beforeplotdata$y_pos[beforeplotdata$Measurement == "BD_TP"] + 
  beforeplotdata$Cont[beforeplotdata$Measurement == "NaOH_SRP"]
beforeplotdata$y_pos[beforeplotdata$Measurement == "NaOH_NRP"] = beforeplotdata$y_pos[beforeplotdata$Measurement == "NaOH_SRP"] + 
  beforeplotdata$Cont[beforeplotdata$Measurement == "NaOH_NRP"]
beforeplotdata$y_pos[beforeplotdata$Measurement == "Total_NE"] = beforeplotdata$y_pos[beforeplotdata$Measurement == "NaOH_NRP"] + 
  beforeplotdata$Cont[beforeplotdata$Measurement == "Total_NE"]



seq_before <- ggplot( beforeplotdata, 
                    aes( x=Treatment, y = Cont, fill = Measurement),
                    scale_x_discrete(position = 'top')) +
  geom_col(
    mapping = aes( y = Cont, fill = Measurement  ),
    stat = "identity", position = position_stack(reverse=TRUE))+
  geom_errorbar(data = beforeplotdata, aes(ymin = y_pos - sd, ymax = y_pos + sd), width = .3, position = position_dodge(0.4)) +
  scale_fill_manual(values = unlist(colors_Bipy), labels = legend_labels_Bipy, name = "Extracted Fraction") +
  coord_flip()+
  labs(y = expression(paste("Extracted P (mg gDW"^-1,")" )),
       x = "Depth (cm)"
  ) +
  #facet_grid(~ Treatment ,labeller= variable_labeller) +
  theme_classic()+
  theme(
    axis.title=element_text(size=3),plot.title = element_text(hjust = 0.5),
    axis.text=element_text(size=14,angle = 0, hjust = 0.5),
    legend.position = "none",
    panel.background = element_rect(colour = "grey20",linewidth = 1)) 
show(seq_before)

ggsave("SeqExtr_Before_Incubation.png", plot = seq_before, width = 5, height = 1.5)

###### KWA
select(kwa, Sample, Total_P, Total_Fe) 

totalElements <- c("Total_Ca","Total_Mn","Total_TP", "Total_Fe", "Total_S", "Total_Al","Total_Ti")
Elements <- totalElements %>% str_remove("Total_")

kwaplotdata <- filter(conc, Measurement %in% totalElements, Time == "after")

kwabefore <-  filter(conc, 
                     Measurement %in% totalElements, 
                     Time == "before") %>% 
  group_by(Parameter,Sample) %>% 
  summarise(Value= mean(Cont)) %>% 
  transform( Parameter = factor(Parameter, levels = Elements))

kwa_plot <- ggplot( kwaplotdata, 
                    mapping =  aes( x=Depth, y = Cont, color = Treatment)) +
  geom_line()+
  geom_point(size= 2) +
  geom_hline( 
    data =  filter(kwabefore,Sample == "Shallow"), 
    aes( yintercept = Value),
    color=c("grey"))+
  geom_hline( 
    data =  filter(kwabefore,Sample == "Deep"), 
    aes( yintercept = Value),
    color=c("grey"))+
  coord_flip()+
  scale_x_reverse() + 
  labs(y = expression(paste(" conc mg/g " )),
       x = "Depth (cm)"
  ) +
  facet_grid(.~ Parameter, scales = "free") +
  theme_classic()+
  theme(
    axis.title=element_text(size=20),plot.title = element_text(hjust = 0.5),
    axis.text=element_text(size=14,angle = 0, hjust = 0.5),
    legend.title = element_text( size = 20),
    panel.background = element_rect(colour = "grey20",linewidth = 1),
    strip.text = element_text(size=13, face="bold", hjust = 0.5) ,
    strip.background = element_blank(),
    legend.text = element_text(size = 15),
    legend.key.size = unit(1.2, "cm"),
    legend.background = element_rect(colour='grey')) 
show(kwa_plot)

##how much mixed?
endmembers <- before %>% ungroup %>% 
  select(-Sample,-sd) %>% 
  pivot_wider(names_from = Treatment, values_from = Cont)
mixdata <- conc %>% left_join(endmembers) %>% 
  filter(Fraction == "Total", Time == "after") %>% 
  mutate(rdeep = (Cont-S)/(D-S))

mix_plot <- ggplot( mixdata, 
                    aes( x=Depth, y = rdeep, color = Parameter),
                    scale_x_discrete(position = 'top')) +
  geom_line( )+
  geom_point()+
  coord_flip()+
  scale_x_reverse() + 
  labs(y = expression(paste(" ratio of deep sediment " )),
       x = "Depth (cm)"
  ) +
  facet_grid(Treatment~Parameter  , scales = "free_x") +
  theme_classic()+
  theme(
    axis.title=element_text(size=20),plot.title = element_text(hjust = 0.5),
    axis.text=element_text(size=14,angle = 0, hjust = 0.5),
    legend.title = element_text( size = 20),
    panel.background = element_rect(colour = "grey20",linewidth = 1),
    strip.text = element_text(size=13, face="bold", hjust = 0.5) ,
    strip.background = element_blank(),
    legend.text = element_text(size = 15),
    legend.key.size = unit(1.2, "cm"),
    legend.background = element_rect(colour='grey')) 
show(mix_plot)

load_from_sed <-function(parameter, measurement, mm){ 
  load <-  mixdata %>% 
  filter(Parameter == "Fe" | Parameter == "Mn" | Parameter == "Al" | Parameter == "Ti"| Parameter == parameter) %>% 
    select(Treatment,Fraction, Sample,Depth,Parameter,rdeep, D, S)%>% 
    pivot_wider(names_from = Parameter, values_from = c(rdeep,D,S)) %>% 
    mutate(rmean = (rdeep_Al+rdeep_Fe+rdeep_Mn+rdeep_Ti)/4 ) %>% 
    right_join(filter(conc, Measurement == measurement)) %>% 
    mutate(theor = rmean*D_TP + (1-rmean)*S_TP) %>% 
    left_join(porosity) %>% 
    mutate(mg = (theor-Cont)*(1-Porosity)*volume_cylinder(3,0.5)) %>% 
    group_by(Treatment) %>% 
    summarise(mgP = sum(1000*mg)/(mm*(3^2)*pi))

  return(load)
}

P_load_from_sed <- load_from_sed("TP","Total_TP",mmP)
S_load_from_sed <- load_from_sed("S","Total_S", mmS)

## Fe plots


stat_deep <- filter(conc, Measurement %in% c( "Total_Fe"), 
                    Treatment == "A" & Depth > 2 | Treatment == "B" | Treatment == "D"
)
stat_shallow <- filter(conc, Measurement %in% c( "Total_Fe"), 
                      Treatment == "A" & Depth < 2 | Treatment == "C" | Treatment == "S"
)

stat_total <-  conc %>% 
  mutate(Type = case_when(Treatment == "D" ~ "D",
                          Treatment == "S" ~  "S",
                          Depth < 2 ~ "S",
                          Depth > 2 ~ "D"
          ) ) %>% 
  group_by(Time, Treatment,Measurement, Type) %>% 
  summarise(sd = sd(Cont), avg = mean (Cont)) %>% 
  filter(Measurement %in% c( "Total_TP", "Total_Fe", "Total_S"))

result_anova <- aov(Cont ~ Treatment, data = filter(stat_shallow))
summary(result_anova)
tukey_results <- TukeyHSD(result_anova)

mdtable(stat_total)
kable(stat_total, format = "markdown")

# Print the results
print(tukey_results)

Feplotdata <- filter(conc, Measurement %in% c("Bipy_TP","BD_TP","Bipy_Fe","BD_Fe", "Total_TP", "Total_Fe", "Total_S"), Time != "before" ) %>% 
            select(-Measurement) %>% 
            pivot_wider(names_from = Parameter, values_from = Cont) %>% 
            mutate(FeP = (Fe/mmFe)/ (TP/mmP) ,
                   FeS = (Fe/mmFe)/ (S/mmS)) %>% 
            transform(Fraction = factor(Fraction, 
                                         levels = c("Bipy","BD","Total")),
                       Treatment = factor(Treatment, levels = (c("A", "B","C"))))

FePstat <-  Feplotdata %>% group_by(Treatment,Fraction) %>% summarise(sd = sd(FeP), mFeP = mean(FeP))

FeP_plot <- ggplot( Feplotdata, 
                    aes( x=Depth, y = FeP, color = Fraction),
                    scale_x_discrete(position = 'top')) +
  geom_line( mapping = aes( y = FeP, color = Fraction  ))+
  geom_point( mapping = aes( y = FeP, color = Fraction  ))+
  scale_color_manual(values = unlist(colors_Bipy2), labels = legend_labels_Fe, name = "Extracted Fraction") +
  coord_flip()+
  scale_x_reverse() + 
  labs(y = expression(paste(" Fe/P " )),
       x = "Depth (cm)"
  ) +
  facet_grid(~ Treatment   ,labeller= variable_labeller) +
  theme_classic()+
  theme(
    axis.title=element_text(size=20),plot.title = element_text(hjust = 0.5),
    axis.text=element_text(size=14,angle = 0, hjust = 0.5),
    legend.title = element_text( size = 20),
    panel.background = element_rect(colour = "grey20",linewidth = 1),
    strip.text = element_text(size=13, face="bold", hjust = 0.5) ,
    strip.background = element_blank(),
    legend.text = element_text(size = 15),
    legend.key.size = unit(1.2, "cm"),
    legend.background = element_rect(colour='grey')) 
show(FeP_plot)

FeP_plotrep <- reposition_legend(FeP_plot, 'bottom right', offset=0.08, panel = 'panel-1-3')
ggsave("FeP_Incubation.png", plot = FeP_plotrep, width = 10, height = 7)