#DAY 3
#Mapping with Google
#AYESHA HARGEY
#3650393
#31 January 2019 

#Homework: convert CSV to .Rdata. Write one line of code to do this.
#First load your CSV into R and then convert.
#Write a 5 line paragraph discusing the library ggsn and library scales. Description.  
#Practice how to read code

# Load libraries
library(tidyverse)
library(ggmap)

# Load data
load("data/cape_point_sites.RData")

cape_point <- get_map(location = c(lon = 18.36519, lat = -34.2352581),
                      zoom = 10, maptype = 'satellite')
# load("data/cape_point.RData")