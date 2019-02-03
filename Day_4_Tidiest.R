#DAY 4
#Tidiest data
#Ayesha Hargey
#3650393
#1 February 2019

#Load libraries 
library(tidyverse) 
library(lubridate)

#Load and rename data
load("data/SACTNmonthly_v4.0.RData")
SACTN <- SACTNmonthly_v4.0

#Remove original
rm(SACTNmonthly_v4.0)

SACTN_depth_mean <- SACTN_depth %>%
  group_by(depth) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE), #assign name to column
            count = n())
SACTN_depth_mean
#mean temperature by depth

ggplot(data = SACTN_depth_mean, mapping = aes(x = depth, y = mean_temp)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE) +
  ggtitle("Temperature and Depth") +
  labs(x = "Depth (m)", y = "Temperature (C)") +
  theme_bw()


SACTN_30_years <- SACTN %>%
  group_by(site, src) %>% #these will be grouped together
  filter(n() > 360) #360 months = 30 years

#VECTOR - PRACTICE THIS
selected_sites <- c("Paternoster", "Oudekraal", "Muizenberg", "Humewood") #making a vector
#creating a set of sites
#this is contrasted to this which is one site:
SACTN %>% 
  filter(site == "Port Nolloth") 

SACTN %>%
  filter(site %in% selected_sites) %>% #the site column in those selected sites
  group_by(site, src) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            sd_temp = sd(temp, na.rm = TRUE))

# [A.A]
# Shows clear understanding of the code, good discriptions
# Neat script
# Trying new things and playing with the different functions and editing by adding new things will only improve you code and marks
# Nice work