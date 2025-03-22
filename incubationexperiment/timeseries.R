library(readxl)
library(tidyverse)
library(ggplot2)
library(lemon)
library(knitr)

mmsulfate <- 96.06
mmP <- 30.97376

# First import all raw data.
times <- read_xlsx("incubationexperiment/timeseries.xlsx")
cores <- read_xlsx("incubationexperiment/cores.xlsx")
conc_sulfide <- read_xlsx("incubationexperiment/Sulfide.xlsx")
conc_sulfate <- read_xlsx("incubationexperiment/Sulfate.xlsx") %>% mutate(Conc = 1000*Conc/mmsulfate)
conc_P <- read_xlsx("incubationexperiment/P.xlsx") %>% mutate(Conc = 1000*Conc/mmP)

conc <- bind_rows("Sulfide" = conc_sulfide, "Sulfate" =  conc_sulfate, "SRP" = conc_P, .id = "Parameter") %>%
  left_join(times, by = c("Timestep")) %>% 
  left_join(cores, by = c("Sample")) %>% 
  filter(Sample != "blank") %>% 
  mutate(Treatment = substr(Sample,1,1))
  # transform(Fraction = factor(Fraction, levels = c("H2O","BD","NaOH")))%>%

conctime <- conc %>% group_by(Parameter, Time, Treatment) %>% 
  summarise(sd = sd(Conc, na.rm=T),Conc = mean(Conc))
 
rates <- conc %>% 
  filter(!is.na(Conc)) %>% 
  #filter(Parameter == "Sulfate") %>% 
  arrange(Parameter, Sample, Timestep, Conc) %>% 
  group_by(Parameter, Sample) %>% 
  mutate(Volume = hwater*(3^2)*pi,
         Start = case_when(Parameter == "Sulfate" ~ (lag(Volume)-20)*lag(Conc)/1000 + lag(Add)*10007.972/mmsulfate + ((20-lag(Add))*lag(clake))/mmsulfate ,
                           T ~  (lag(Volume)-20)*lag(Conc)/1000),
         End = Volume*Conc/1000,
         diff = (End-Start)/((3^2)*pi))%>% 
  filter(!is.na(diff)) %>%  
mutate(add = cumsum(diff),
       timediff = Time - lag(Time),
       flux = diff/(timediff))  %>% ungroup


  

ratestable <- rates %>% filter(Time > 10) %>% 
  group_by(Parameter, Treatment)%>% 
  summarise(total = sum(diff, na.rm = T)/3,sdflux1 = sd(flux, na.rm=T),flux = mean(flux, na.rm = T)) 

 ratestable2 <- rates %>% 
   group_by(Parameter, Treatment, Sample) %>% 
   summarise(total = sum(diff, na.rm = T)) %>% 
  summarise(sdtot = sd(total), meantot = mean(total))


kable(ratestable2, format = "markdown")

566# Labels and colors
legend_labels <- c("Vivianite+OM", "control vivianite", "control OM", "4")
colors <- list(c("#E69F00", "#56B4E9", "#009E73", 
                 "#F0E442"))

# Plot the sequential extractions with error bars

ggplot(filter(rates, Time > 3), aes(x = flux, fill = Treatment, color = Treatment)) +
  geom_histogram(binwidth = .1, position = "identity", alpha=0.5) +
  facet_wrap( ~ Parameter, scales = "free") +
  labs(title = "Histograms", x = "Value", y = "Frequency")

result_anova <- aov(flux ~ Treatment, data = filter(rates, Time > 3, Parameter == "Sulfate"))
summary(result_anova)
# Perform Tukey HSD post hoc test
tukey_results <- TukeyHSD(result_anova)

# Print the results
print(tukey_results)

ratestimeseries <- ggplot(rates, mapping = aes(x = Time, y = add, color = Treatment, by = Sample)) +
  geom_path(na.rm = T) +
  geom_point(na.rm = T) +
  #geom_errorbar( aes(ymin = Conc - sd, ymax = Conc + sd), width = 1, position = position_dodge(0.1)) +
  scale_color_manual(values = unlist(colors), labels = legend_labels, name = "treatment") +
  guides(color = guide_legend(label.position = "top", label.hjust = 1)) +
  labs(y = expression(paste("cumulative load (",mu,"mol cm"^{-2},") ")),
       x = "Time (days)") +
  theme_bw() +
  facet_grid(rows = "Parameter", scale = "free_y")+
  theme(
    axis.title = element_text(size = 18),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 16, angle = 0, hjust = 0.5),
    strip.background = element_blank(),
    strip.text.y = element_text(angle= 0, size=13, face="bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    legend.key.size = unit(1, "cm"),
    legend.position = "top"
  )
show(ratestimeseries)

timeseriesrep <- reposition_legend(ratestimeseries, 'top left', offset=0.002, panel = 'panel-1-1')
ggsave("inc_timeseriescum.png", plot = timeseriesrep, width = 13, height = 8)

200*mmP/1000

timeseries <- ggplot(na.omit(conctime), mapping = aes(x = Time, y = Conc, color = Treatment)) +
  geom_path(na.rm = T) +
  geom_point(na.rm = T) +
  geom_errorbar( aes(ymin = Conc - sd, ymax = Conc + sd), width = 1, position = position_dodge(0.1)) +
  scale_color_manual(values = unlist(colors), labels = legend_labels, name = "treatment") +
  guides(color = guide_legend(label.position = "top", label.hjust = 1)) +
  labs(y = expression(paste("Concentration (umol L"^{-1}, ")")),
       x = "Time (days)") +
  theme_bw() +
  facet_grid(rows = "Parameter", scale = "free_y")+
  theme(
    axis.title = element_text(size = 18),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 16, angle = 0, hjust = 0.5),
    strip.background = element_blank(),
    strip.text.y = element_text(angle= 0, size=13, face="bold", hjust = 0.5),
    legend.title = element_blank(),
    legend.text = element_text(size = 6),
    legend.key.size = unit(1, "cm"),
    legend.position = "top"
  )

  
filter(rates, Time > 7)

  dodge <- position_dodge(width=10)
  fluxseries<- ggplot(filter(rates, Time > 8), aes(x = Time, y = flux, fill = Treatment)) +
    geom_bar(stat = "identity", position=position_dodge(), width = 4) +
    geom_errorbar(
      aes(ymin = flux - sd, ymax = flux + sd, color= Treatment),
      position = position_dodge(),
      width = 0.5)+
    #geom_errorbar(data = conc, aes(ymin = y_pos - sd, ymax = y_pos + sd), width = 1, position = position_dodge(0.1)) +
    scale_color_manual(values = unlist(colors), labels = legend_labels, name = "treatment") +
    labs(y = expression(paste("Flux in umol d"^{-1}, " cm"^{-2})),
         x = "Time (days)") +
    theme_bw() +
    facet_grid(rows = "Parameter", scale = "free_y")
  theme(
    axis.title = element_text(size = 20),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 16, angle = 0, hjust = 0.5),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 10),
    legend.key.size = unit(1.3, "cm")
  )
  

show(fluxseries)
# Display the plot
print(fluxseries)
ggsave("fluxseries.png", plot = fluxseries, width = 20, height = 7)
