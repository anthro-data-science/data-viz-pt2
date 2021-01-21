# R code adapted from Shane Scaggs with Delaney's additions for Data Viz Workshop 2 Jan 2021

library(tidyverse)
library(rethinking)

data("starwars")
starwars <- starwars

head(starwars)

#### Ordered and themed bar graph ####

# basic bar graph 
bargraph <- starwars %>% ggplot(aes(x=reorder(eye_color, eye_color, length))) + 
  geom_bar(fill="grey12") + 
  labs(x="eye color")

# use minimal theme
bargraph + theme_minimal() 

# add to a default theme
bargraph + theme_minimal() + 
  theme(panel.background = element_rect(color='black'))

# make a custom theme 
bargraph + theme(
  panel.background = element_rect(fill=rethinking::col.alpha('cornsilk1')), 
  panel.grid.major = element_line(color='cornsilk2'), 
  panel.grid.minor = element_line(color='cornsilk2', linetype = 2))

# save custom theme 
my_theme <-theme(
  panel.background = element_rect(fill=adjustcolor('cornsilk1', alpha.f = 0.5)), 
  panel.grid.major = element_line(color='cornsilk2'), 
  panel.grid.minor = element_line(color='cornsilk2', linetype = 2)
)

#### Graphing distributions ####

# histogram of height
histogram <- starwars %>% ggplot(aes(height)) + my_theme + 
  geom_histogram(binwidth = 10,
                 fill="burlywood4", color="black") +
  labs(x="Height", y="Frequency", fill="burlywood4")

# the minor ticks on the x axis should match the binwidth 
# Delaney additions: change fill color for bars, axis ticks, and axis labels
histogram + scale_x_continuous(breaks=seq(0,300,20)) +
theme(axis.text.x = element_text(face="bold", color="orange4",size=12)) +
theme(axis.text.y = element_text(face="bold", color="orange4", size=12)) +
theme(axis.title.x = element_text(color="darkorange4", size=14, face="bold")) +
theme(axis.title.y = element_text(color="darkorange4", size=14, face="bold"))

# geom area 
starwars %>% 
  ggplot(aes(mass)) + my_theme + 
  geom_area(stat='bin') 

# fill by factor 
a <- na.omit(starwars) %>% #omit NA's so they dont show up in legend (Delaney)
  # remove jabba, he's so big 
  filter(!grepl('Jabba', name)) %>% 
  ggplot(aes(mass, fill=gender)) + my_theme + 
  geom_area(stat='bin', binwidth=10, alpha=0.9) + 
  scale_x_continuous(breaks=seq(0,300,20))

# manually change the fill colors + axis labels (Delaney addition)
a + scale_fill_manual(values = c('darkseagreen','violetred')) +
  theme(axis.text.x = element_text(color="black",size=12)) +
  theme(axis.text.y = element_text(color="black", size=12)) +
  theme(axis.title.x = element_text(color="black", size=14, face="bold")) +
  theme(axis.title.y = element_text(color="black", size=14, face="bold")) +
  theme(legend.title = element_text(color="black", size=12, face="bold"))
  
#### Plot two discrete variables #### 

# tile plot, kind of like two histograms making a raster
discrete <- starwars %>% ggplot(aes(eye_color, skin_color)) + 
  geom_bin2d() +
  theme_minimal() 

# change the gradient colors to make break more obvious 
discrete + scale_fill_continuous(breaks = seq(0,10,2), low="#3b55ff", high="tomato")

### Trend line and scatter plots #### 

# height and mass
hx <- starwars %>% 
  filter(!grepl('Jabba', name)) %>% 
  ggplot( aes(height, mass)) + 
  geom_smooth(color='blue', alpha=0.2) + 
  theme_minimal() + 
  theme(panel.background = element_rect(color="black")); hx

# overlay points
hx + geom_point(pch=21)

#### Use contours to visualize continuous variables with outliers ####
starwars %>% ggplot( aes(mass, height )) + 
  geom_point() + 
  theme_minimal() + 
  scale_x_continuous(breaks = seq(0,150,25))

# density_2d
starwars %>% ggplot( aes(mass, height )) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") + 
  theme_minimal() 

# some aesthetic changes
starwars %>% ggplot( aes(mass, height )) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon") + # fill mapped to density 
  theme_minimal() + 
  scale_x_continuous(breaks = seq(0,150,25)) + 
  scale_fill_gradient(low="#3b55ff", high="tomato") +  # gradient colors
  scale_y_continuous(breaks = seq(0,250,25)) + 
  theme(
    panel.background = element_rect(color="black",fill="#121a4d"),
    panel.grid = element_line(color="#001a4d")  # some custom colors
  )





