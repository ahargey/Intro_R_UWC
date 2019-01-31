#DAY 3
#Mapping with style
#AYESHA HARGEY
#3650393
#31 January 2019 

#Homework: convert CSV to .Rdata. Write one line of code to do this.
#First load your CSV into R and then convert.
#Write a 5 line paragraph discusing the library ggsn and library scales. Description.  
#Practice how to read code

# Load libraries
library(tidyverse)
library(scales)
library(ggsn)

# Load Africa map
load("data/africa_map.RData")

ggplot() +
  borders() + # The global shape file
  coord_equal() # Equal sizing for lon/lat 

sa_1 <- ggplot() +
  borders(fill = "grey70", colour = "black") +
  coord_equal(xlim = c(12, 36), ylim = c(-38, -22), expand = 0) # Force lon/lat extent
sa_1

sa_2 <- sa_1 +
  annotate("text", label = "Atlantic\nOcean", #\n means new line
           x = 15.1, y = -32.0,
           size = 5.0,
           angle = 30,
           colour = "orchid1") +
  annotate("text", label = "Indian\nOcean",
           x = 33.2, y = -34.2,
           size = 5.0,
           angle = 330,
           colour = "limegreen")
sa_2

sa_3 <- sa_2 + #the second SOUTH AFRICA map
  scalebar(x.min = 32, x.max = 26, y.min = -34, y.max = -35, # Set location of bar
           dist = 200, height = 1, st.dist = 0.8, st.size = 4, # Set particulars
           dd2km = TRUE, model = "WGS84") + # Set appearance #code doesn't change
  north(x.min = 28.5, x.max = 25.5, y.min = -33, y.max = -31, # Set location of symbol
        scale = 1.2, symbol = 16) #you shift these by adjusting the x.min and y.min values
sa_3

sa_4 <- sa_3 +
  annotation_custom(grob = ggplotGrob(africa_map), #this is the inset map feature
                    xmin = 20.9, xmax = 26.9,
                    ymin = -30, ymax = -24)
sa_4

