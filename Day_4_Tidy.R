# DAY 4
# Tidy data
# Ayesha Hargey
# 3650393
# 1 February 2019

#load libraries
library(tidyverse)

load("data/SACTN_mangled.RData") 
#coastal data is important to compare open ocean to coast
#coastal area is affected differently

ggplot(SACTN1, aes(x = date, y = temp)) + 
  geom_line(aes(colour = site, group = paste0(site,src))) + #see note at bottom
  labs(x = "Date", y = "Temperature (C)") +
  ggtitle("Coastal Temperature") +
  theme_bw()
#grouped by site with a different colour. when you group 
#by multiple variables, you use paste0 function.
#group by site and source

SACTN2_tidy <- SACTN2 %>% 
  gather(DEA, KZNSB, SAWS, key = "src", value = "temp")
#first bit is the columns you want to gather
#key is the new column to explain the numbers (key as in legend)
#value is where the numbers are going

#FUNCTIONS

#GATHER
SACTN3_tidy <- SACTN3 %>% #this data has a variable and a value column
  spread(key = var, value = val) #should be separated

#SEPARATE
SACTN4a_tidy <- SACTN4a %>%
  separate(col = index, into = c("site", "src"), sep = "/")
#if you split anything into more than one variable you use the concatanate function c()

#UNITE
SACTN4b_tidy <- SACTN4b %>%
  unite(year, month, day, col = "date", sep = "-") #the 'sep' determines what separates them

#JOINING
SACTN4_tidy <- left_join(SACTN4a_tidy, SACTN4b_tidy)
