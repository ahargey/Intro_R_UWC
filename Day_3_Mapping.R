#DAY 3
#Mapping with ggplot2
#AYESHA HARGEY
#3650393
#31 January 2019 

#Homework: convert CSV to .Rdata. Write one line of code to do this.
#First load your CSV into R and then convert.

#Load libraries 
library(tidyverse)
library(ggpubr)

#Load data
#These data used to be .csv's but now are RData
load("data/south_africa_coast.RData")
load("data/sa_provinces.RData")
load("data/rast_annual.RData")
load("data/MUR.RData")
load("data/MUR_low_res.RData")

sst <- MUR_low_res #sst stands for sea surface temperature

cols11 <- c("#004dcd", "#0068db", "#007ddb", "#008dcf", "#009bbc", #stores colour
            "#00a7a9", "#1bb298", "#6cba8f", "#9ac290", "#bec99a") #custom palette


ChickWeight <- datasets::ChickWeight #adds Chickweight to environment

#CHICKEN WEIGHT
ggplot(data = ChickWeight, aes(x = Time, y = weight)) +
  geom_point()

#SOUTH AFRICAN COAST 
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) + #coast_outline
  geom_point() +
  labs(x = "longititude", y = "latitude") +
  ggtitle("Sea Surface Temperature")

#LANDMASK
ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) # The land mask #group = group the first is a function, the second is the column

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group))

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) # Force lon/lat extent
  #coord_equal limits the co-ordinates by showing the minimum and maximum x and y value)

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) + # The ocean temperatures. Bins are the category that temperatures falll into.
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) #uses default colours

ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  scale_fill_manual("Temp. (°C)", values = cols11) + # Set the colour palette // The palette constructed earlier // Scale fill gives a name 
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) 

final_map <- ggplot(data = south_africa_coast, aes(x = lon, y = lat)) +
  geom_raster(data = sst, aes(fill = bins)) +
  geom_polygon(colour = "black", fill = "grey70", aes(group = group)) +
  geom_path(data = sa_provinces, aes(group = group)) +
  geom_tile(data = rast_annual, aes(x = lon, y = lat, fill = bins),
            colour = "white", size = 0.1) +
  scale_fill_manual("Temp. (°C)", values = cols11) +
  coord_equal(xlim = c(15, 34), ylim = c(-36, -26), expand = 0) +
  scale_x_continuous(position = "top") + # Put x axis labels on top of figure
  theme(axis.title = element_blank(), # Remove the axis labels
        legend.text = element_text(size = 7), # Change text size in legend
        legend.title = element_text(size = 7), # Change legend title text size
        legend.key.height = unit(0.3, "cm"), # Change size of legend
        legend.background = element_rect(colour = "white"), # Add legend background
        legend.justification = c(1, 0), # Change position of legend
        legend.position = c(0.55, 0.4)) # Fine tune position of legend
final_map

#[A.A]
# Self made comments are always better to have when studying
# Neat script
# Try new things and playing with functions in R will only improve your code and your marks
## Script runs fully