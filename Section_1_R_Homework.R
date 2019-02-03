#SECTION 1
#R Homework
#AYESHA HARGEY
#3650393
#3rd February 2019

# Section 1: always make maps for both rast_feb and rest_aug
# Make use of the rast_feb and rast_aug dataset:
# Explore the dataset (Hint* head, tail, !dims, glimpse, !summary etc) - Make use of google for more functions on exploring a dataset
# Create a map by making use of the lat and long variables
# Create a colour pallete using the link in the document and make use this colour pallete on the map
# Add complete labels and titles to the map
# Add the name of the oceans (Atlanic and indian ocean) on the map, increase the size of the labels
# The map should include the north arrow and scale bar
# Bonus marks for insetting (having a smaller map inside another map)
# Get creative, try new things.

#Load libraries
library(tidyverse)
library(ggpubr)
library(scales)
library(ggsn)

#Load data
#As it is already in a .RData format, there's no conversion necessary 
load("data/rast_feb.RData")
load("data/rast_aug.RData")

#Renaming data for efficency
feb <- rast_feb
aug <- rast_aug

#Removing duplicate data
remove(rast_feb)
remove(rast_aug)

#Exploring the dataset 
#FEBRUARY

glimpse(feb) #overall preview of data, shows every column
head(feb) #first six rows
tail(feb) #last six rows
nrow(feb) #number of row
ncol(feb) #number of columns
dim(feb) #the dimensions of the data i.e how many rows and columns 
any(is.na(feb)) #is there any missing data?
summary(feb) #summary of the data according to quartiles and min/max values

#GENERATING PALETTE
palette <- c("#81A5D1","#9B9DCF","#B294C7","#C68BBB",
             "#D583AC","#E07D9A","#E57987","#E47974")
#palette generated from Colorpicker

feb_map_process <-  ggplot(data = feb, aes(x = lon, y = lat)) +
  geom_raster(aes(x = lon, y = lat, fill = bins)) +
  geom_tile(data = feb, aes(x = lon, y = lat, fill = bins), width = 1.5) +
  geom_polygon(colour = "black", fill = "grey60") +
  scale_fill_manual("Temperature (°C)", values = palette) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Coastal Temperatures for the month of February") +
  theme(axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        plot.background = element_rect(fill = "#d4ccc7"),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 10), # Change text size in legend
        legend.title = element_text(size = 10), # Change legend title text size
        legend.key.height = unit(0.5, "cm"), # Change size of legend
        legend.background = element_rect(colour = "white"), # Add legend background
        legend.justification = c(1, 0.3), # Change position of legend
        legend.position = c(1, 0.38))
feb_map_process

#LABELS
feb_map_labelled <- feb_map_process +
  annotate("text", label = "Atlantic\nOcean", #\n means new line
           x = 16, y = -33.0,
           size = 5.5,
           angle = 330,
           colour = "palegreen",
           fontface = "bold") +
  annotate("text", label = "Indian\nOcean",
           x = 27.5, y = -34.5,
           size = 5.5,
           angle = 30,
           colour = "palevioletred1",
           fontface = "bold")
feb_map_labelled

#NORTH ARROW AND SCALE
feb_map_scale <- feb_map_labelled +
  scalebar(x.min = 33, x.max = 35, y.min = -34.5, y.max = -33.5, # Set location of bar
           dist = 200, height = 0.2, st.dist = 0.2, st.size = 3.5, # Set particulars
           dd2km = TRUE, model = "WGS84") + # Set appearance #code doesn't change
  north(x.min = 15.5, x.max = 16.5, y.min = -28, y.max = -27, # Set location of symbol
        scale = 1, symbol = 3) #you shift these by adjusting the x.min and y.min values
feb_map_scale

#INSETTING
load("data/africa_map.RData")
feb_map_final <- feb_map_scale +
  annotation_custom(grob = ggplotGrob(africa_map),
                  xmin = 23, xmax = 27,
                  ymin = -31.5, ymax = -28.5)
feb_map_final

#AUGUST 
glimpse(aug) #overall preview of data, shows every column
head(aug) #first six rows
tail(aug) #last six rows
nrow(aug) #number of row
ncol(aug) #number of columns
dim(aug) #the dimensions of the data i.e how many rows and columns 
any(is.na(aug)) #is there any missing data?
summary(aug) #summary of the data according to quartiles and min/max values

aug_map_process <-  ggplot(data = aug, aes(x = lon, y = lat)) +
  geom_raster(aes(x = lon, y = lat, fill = bins)) +
  geom_tile(data = aug, aes(x = lon, y = lat, fill = bins), width = 1.5) +
  geom_polygon(colour = "black", fill = "grey60") +
  scale_fill_manual("Temperature (°C)", values = palette) +
  labs(x = "Longitude", y = "Latitude") +
  ggtitle("Coastal Temperatures for the month of August") +
  theme(axis.title.x = element_text(face = "bold", size = 12),
        axis.title.y = element_text(face = "bold", size = 12),
        plot.background = element_rect(fill = "#E2C0A5"),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 10), # Change text size in legend
        legend.title = element_text(size = 10), # Change legend title text size
        legend.key.height = unit(0.5, "cm"), # Change size of legend
        legend.background = element_rect(colour = "white"), # Add legend background
        legend.justification = c(1, 0.3), # Change position of legend
        legend.position = c(1, 0.38))
aug_map_process

#LABELS
aug_map_labelled <- aug_map_process +
  annotate("text", label = "Atlantic\nOcean", #\n means new line
           x = 16, y = -33.0,
           size = 5.5,
           angle = 330,
           colour = "seagreen1",
           fontface = "bold") +
  annotate("text", label = "Indian\nOcean",
           x = 27.5, y = -34.5,
           size = 5.5,
           angle = 30,
           colour = "violetred1",
           fontface = "bold")
aug_map_labelled

#NORTH ARROW AND SCALE
aug_map_scale <- aug_map_labelled +
  scalebar(x.min = 33, x.max = 35, y.min = -34.5, y.max = -33.5, # Set location of bar
           dist = 200, height = 0.2, st.dist = 0.2, st.size = 3.5, # Set particulars
           dd2km = TRUE, model = "WGS84") + # Set appearance #code doesn't change
  north(x.min = 15.5, x.max = 16.5, y.min = -28, y.max = -27, # Set location of symbol
        scale = 1, symbol = 3) #you shift these by adjusting the x.min and y.min values
aug_map_scale

#INSETTING
aug_map_final <- aug_map_scale +
  annotation_custom(grob = ggplotGrob(africa_map),
                    xmin = 23, xmax = 27,
                    ymin = -31.5, ymax = -28.5)
aug_map_final
