# Some ggplot2 graphs using Stars Wars data 

#install.packages('tidyverse')
library(tidyverse)

data("starwars")

#### Ordered and themed bar graph ####

# basic bar graph 
starwars %>% 
    ggplot( aes( x=eye_color )) + 
    geom_bar( fill='black' ) + 
    theme_minimal() + 
    coord_flip( )
#ggsave(filename = "BasicBarGraph.png", height = 3, width = 4, dpi = 600)

# basic bar graph 
my_theme <- theme( 
    panel.background = element_rect(color='black',
                                    fill='white'), 
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.x = element_line(linetype=2)
)

starwars %>% 
    ggplot(aes( x=reorder(eye_color, 
                          eye_color, 
                          length ))) + 
    geom_bar( fill='black' ) + 
    theme_minimal() + 
    coord_flip( ) + 
    my_theme + 
    labs( x = "eye color")

##ggsave(filename = "OrderedBarGraph.png", height = 3, width = 4, dpi = 600)


#### Plot two discrete variables #### 

# tile plot, kind of like two histograms making a raster
starwars %>% 
    ggplot(aes(eye_color, skin_color)) + 
    geom_bin2d() +
    theme_minimal() + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0)) 

##ggsave("BasicBin2D.png", height = 6, width = 8, dpi=600)

# change the gradient colors to make break more obvious 
starwars %>% 
    ggplot(aes(eye_color, skin_color)) + 
    geom_bin2d() +
    theme_minimal() + 
    theme(axis.text.x=element_text(angle = -90, hjust = 0)) + 
    scale_fill_continuous(breaks = seq(0,10,2), low="#3b55ff", high="tomato")
##ggsave("NewColorsBin2D.png", height = 6, width = 8, dpi=600)

#### Bin2d with Ordinal variables 

# create new variables 
starwars$ord_mass <- with(starwars, 
                          cut(mass, 
                              breaks=quantile(mass, 
                                              breaks=seq(0,1,length.out=5),
                                              na.rm=T),
                              include.lowest=TRUE, 
                              labels=c(1,2,3,4)))

starwars$ord_height <- with(starwars, 
                          cut(height, 
                              breaks=quantile(height, 
                                              breaks=seq(0,1,length.out=5),
                                              na.rm=T),
                              include.lowest=TRUE, 
                              labels=c(1,2,3,4)))
starwars %>% 
    ggplot(aes(ord_mass, ord_height)) + 
    geom_bin2d(binwidth=1) +
    theme_minimal() + 
    scale_fill_continuous(breaks = seq(0,10,2), 
                          low='#3b55ff', high='tomato') + 
    stat_bin2d(geom='text', aes(label = ..count..), 
               binwidth = 1, color='white') 
#ggsave("OrdinalBin2d.png", height = 3, width = 4, dpi=600)

#### Use contours to visualize continuous variables with outliers ####
set.seed(777)
N <- 3000
R <- 1.1
x <- rnorm(N, 2, 2)
y <- R*x*(1-x) + rnorm(N)

d <- data.frame(x,y)
dim(d)
head(d)

d %>% 
    ggplot( aes( x,y )) +
    geom_point() + 
    theme_minimal() + my_theme
#ggsave("SimScatter.png", height = 3, width = 4, dpi=600)

d %>% 
    ggplot( aes( x,y )) +
    stat_density_2d( aes(fill = ..level..), geom = 'polygon') + 
    theme_minimal() + theme(legend.position = 'none') + my_theme
#ggsave("SimContour.png", height = 3, width = 4, dpi=600)


# scatter with starwars
starwars %>% 
    filter(!mass > 200) %>%
    ggplot( aes(mass, height )) + 
    geom_point( pch=21 ) + 
    theme_minimal() + my_theme
#ggsave("BasicScatter.png", height=3, width=4, dpi=600)

# density_2d instead
starwars %>% 
    ggplot( aes(mass, height )) +
    stat_density_2d( aes(fill = ..level..), 
                     geom = "polygon") + 
    theme_minimal() + 
    theme(legend.position = 'none') + my_theme  
#ggsave("BasicContour.png", height=3, width=4, dpi=600)


# some aesthetic changes
d %>% ggplot( aes(mass, height )) +
    stat_density_2d(aes(fill = ..level..), geom = "polygon") + # fill mapped to density 
    theme_minimal() + 
    scale_x_continuous(breaks = seq(0,150,25)) + 
    scale_fill_gradient(low="#3b55ff", high="tomato") +  # gradient colors
    scale_y_continuous(breaks = seq(0,250,25)) + 
    theme(
        panel.background = element_rect(color="black",fill="#121a4d"),
        panel.grid = element_line(color="#001a4d")  # some custom colors
    )
