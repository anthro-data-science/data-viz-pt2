# R code adapted from Shane Scaggs and Monica Keith with Delaney's additions for Data Viz Workshop 2 Jan 2021

library(tidyverse)

data("starwars")
starwars <- starwars

### Trend line and scatter plots #### 

# height and mass
hx <- starwars %>% 
  filter(!grepl('Jabba', name)) %>% #remove Jabba out, mass >1000kg
  ggplot( aes(height, mass)) + 
  geom_smooth(color='purple', alpha=0.2) + 
  theme_minimal() + 
  labs (x="Height", y="Mass") +
  theme(axis.text.x = element_text(color="black",size=10)) +
  theme(axis.text.y = element_text(color="black", size=10)) +
  theme(axis.title.x = element_text(color="black", size=12, face="bold")) +
  theme(axis.title.y = element_text(color="black", size=12, face="bold")) +
  theme(panel.background = element_rect(color="black")); hx

# overlay points
hx + geom_point(pch=21, color="turquoise3") #add whichever color you like best

#### VIOLIN PLOT ####
##body mass distributions of human Starwars characters by sex
head(starwars)

vioplot<- starwars %>% 
  dplyr::select(mass, species, name, sex) %>% #use dplyr::select if error msg with select
  drop_na() %>% 
  filter(species=="Human") %>% 
  ggplot(aes(x=sex, y=mass)) +
  geom_violin(aes(fill=sex, alpha=0.5)) +
  scale_fill_manual(values = c("darkorange4", "darkmagenta")) +
  labs(x="Sex", y="Mass (kg)") +
  ylim(35,145) + 
  ggtitle("Starwars human body mass distributions") +
  theme_minimal() +
  theme(legend.position="none") #remove the legend

##add data points and label outliers
vioplot +
  geom_jitter(height = 0, width = 0.02) + 
  geom_text(aes(label=ifelse(mass>100, as.character(name),'')), vjust=1.0, hjust=0)
#lifelse used for outliers

# option to use size= to adjust geom_text size






