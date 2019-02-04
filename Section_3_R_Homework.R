#SECTION 1
#R Homework
#AYESHA HARGEY
#3650393
#4th February 2019

# Section 3: 
# Make use of the SACTN_day1 data:
# Here create a graph showing temperature variation between sites !(group by site!)
# Select all the temperatures recorded at the site Port Nolloth during August or September.
# Select all the monthly temperatures recorded in Port Nolloth during the year 1994
# Calculate the average temperature by depth
# Work through the tidyverse section within the document. Show what you have done by creating comments/ notes throughout the script
# FROM TIDY TO TIDIEST

#Load libraries
library(tidyverse)
library(ggpubr)
library(lubridate)

#Load data from csv
SACTN <- read_csv("data/SACTN_day_1.csv")

SACTN_grouped <- SACTN %>%  #new dataframe
  group_by(site) 

ggplot(SACTN_grouped, aes(x = date, y = temp, colour = site)) +
  geom_line(aes(linetype = site), # Line type depends on site
            size = 1.5) +
  scale_colour_manual(values = c("slategray1", "orangered3", "lightpink")) +
  labs(x = "Year", y = "Temperature (C°)") +
  ggtitle("Sea Surface Temperature Across Three Sites for the Years 1973-2013") + #title
  theme_bw () +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, colour = "black", size=12),
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.background = element_rect(fill = "#f0eae8"),
        plot.title = element_text(size=16, face="bold", hjust=0.5))

SACTN_PN_August_September <- SACTN %>% 
  filter(site == "Port Nolloth", month(date) == 8 | month(date) == 9)

SACTN_PN_1996 <- SACTN %>% 
  filter(site == "Port Nolloth", year(date) == 1994)

#TIDY DATA
load("data/SACTN_mangled.RData") 
#coastal data is important to compare open ocean to coast
#coastal area is affected differently

ggplot(SACTN1, aes(x = date, y = temp)) + 
  geom_line(aes(colour = site, group = paste0(site,src))) + #see note at bottom
  labs(x = "", y = "Temperature (°C)", colour = "Site") +
  ggtitle("Coastal Temperature") +
  theme_bw()
#grouped by site with a different colour. when you group 
#by multiple variables, you use paste0 function.
#group by site and source

#GATHER
SACTN2_tidy <- SACTN2 %>% 
  gather(DEA, KZNSB, SAWS, key = "src", value = "temp")
#first bit is the columns you want to gather
#key is the new column to explain the numbers (key as in legend)
#value is where the numbers are going

#SPREADING
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

#TIDIER
#Load dataset 
load("data/SACTNmonthly_v4.0.RData")

#Rename the data a shorter name and remove the original
SACTN <- SACTNmonthly_v4.0 #this data is tidy 
rm(SACTNmonthly_v4.0)

SACTN %>%
  filter(site == "Amanzimtoti") #extract from the site column this specific place
#it needed two equal two signs

SACTN %>%
  filter(site == "Pollock Beach", month(date) == 12 | month(date) == 1)
#it extracts all the data from the site column to Pollock Beach
#Select either the 12th month from the date column
#Or the 1st month from the date colum

#ARRANGE
SACTN %>%
  arrange(depth, temp)
#arranges the columns from the lowest to the highest values
#eg: to give a minimum, you can use this function or min()

SACTN %>%
  arrange(desc(temp))
#'desc' arranges from highest to lowest

SACTN %>%
  filter(site == "Humewood", year(date) == 1990)
#only displays the data from the site Humewood that occued in the year 1990

humewood_90s <- SACTN %>%
  filter(site == "Humewood", year(date) %in% seq(1990, 1999, 1)) #new dataframe just of humewood

SACTN %>%
  filter(site == "Port Nolloth", #The site that's being filtered
         src == "DEA", #The source that's being filtered
         temp <= 11 | #Only at temperatures at or below 11 or that
           is.na(temp)) #Includes missing values

SACTN %>% #these are all new dataframes
  select(site, src, date, temp) #Only these 4

#Select all columns between two variables (site and temp) like a sequence
SACTN %>%
  select(site:temp) #site till temp

#Select all columns except the following
SACTN %>%
  select(-date, -depth)

#Select all columns except those in a sequence
#The '-' goes outside of a new set of brackets
#So in between two sets of ()
SACTN %>%
  select(-(date:depth))

SACTN %>% #specifies individual column order
  select(temp, src, date, site)


SACTN %>% #grabs all columns
  select(type, src, everything())

SACTN %>%
  select(temp:type, everything(), -src) #it works in a sequential order 

#CREATING NEW VARIABLES
#MUTATE 
try5 <- SACTN %>%
  mutate(kelvin = temp + 273.15) #new column called 'kelvin'
#the contents of kelvin is what's in the temp column plus the value 

SACTN %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE)) #gives the mean and removes invalid data

SACTN %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            sd_temp = sd(temp, na.rm = TRUE),
            min_temp = min(temp, na.rm = TRUE),
            max_temp = max(temp, na.rm = TRUE))
#specify dataset used
#have to give a name to the column 
#na.rm eliminates n/a results
#can also just use = T instead of = TRUE


#TIDIEST
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