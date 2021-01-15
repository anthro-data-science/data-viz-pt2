###### Monica Keith: plots for data visualization workshop part 2 ######

library(tidyverse); library(ggalluvial)

#### VIOLIN PLOT ####
##body mass distributions of human Starwars characters by sex
head(starwars)

vioplot<- starwars %>% 
  select(mass, species, name, sex) %>% 
  drop_na() %>% 
  filter(species=="Human") %>% 
  ggplot(aes(x=sex, y=mass)) +
  geom_violin(aes(fill=sex, alpha=0.5)) +
  scale_fill_manual(values = c("darkorange2", "darkmagenta")) +
  labs(x="Sex", y="Mass (kg)") +
  ylim(35,145) + 
  ggtitle("Starwars human body mass distributions") +
  theme_minimal() +
  theme(legend.position="none") #remove the legend

##add data points and label outliers
vioplot +
  geom_jitter(height = 0, width = 0.02) + 
  geom_text(aes(label=ifelse(mass>100, as.character(name),'')), vjust=1.0, hjust=0) 


#### FOREST PLOT ####
##Hurricane wind averages and ranges 2010-2015
head(storms)

##tabulate stats from repeated observations 
hurrtab <- storms %>% 
  select(name, year, status, wind) %>% 
  filter(status=="hurricane" & year>2009) %>% 
  mutate(yr_name = paste(year, name, sep = "_")) %>% #storm names repeat, add year to each name
  group_by(yr_name) %>% 
  summarize_each(funs(mean, min, max), wind) %>% #mean, minimum, and maximum wind speed for each hurricane
  ungroup

hurrtab %>% 
  ggplot(aes(x=yr_name, y=mean, ymin=min, ymax=max)) +
  geom_pointrange() +
  ylim(60, 140) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle=90, size=10)) +
  labs(x="Hurricane", y="Wind (km/hr)") +
  ggtitle("2010-2015 Hurricane Wind Ranges")


#### ALLUVIAL PLOT ####
##trends in tropical storm frequencies 1975-2015

##tabulate frequencies of storm types every year, represent each storm by its peak status  
allutab <- storms %>% 
  select(name, year, status, wind) %>%  
  mutate(name = paste(name, year, sep = "_")) %>% #storm names repeat, add year to each name
  group_by(name) %>% 
  slice_max(wind) %>% #storm status peaks at max wind speed
  ungroup %>% 
  group_by(year, status) %>% 
  summarize(n()) #count frequency of peak storm types by year

colnames(allutab)[3] <- "count" #rename variable "n()"

alluplot <- allutab %>% 
  ggplot(aes(x=year, y=count, stratum=status, alluvium=status)) +
  geom_alluvium(aes(fill=status), alpha=0.75, decreasing=FALSE) + #plot strata in ascending frequency order
  scale_x_continuous(breaks=seq(1975,2015,5)) +
  theme_minimal() +
  ggtitle("Tropical storm frequencies 1975-2015")

##edit and move legend
alluplot +
  scale_fill_discrete(name="Peak Status",
                      breaks=c("tropical depression", "tropical storm", "hurricane"), #reorder
                      labels=c("Tropical depression", "Tropical storm", "Hurricane")) + 
  theme(legend.position = c(0.85,0.85), legend.background = element_rect(fill = "white", linetype="solid", color="black"))

##save plot as image
ggsave("alluplot.png", plot=last_plot(), dpi=300, height=5, width=7, units="in")


