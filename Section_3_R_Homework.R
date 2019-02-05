#SECTION 3 
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

#GROUPING DATAFRAMES
SACTN_temp_group <- SACTN %>%
  group_by(round(temp), depth) #grouped by depth and rounded off temperature

SACTN_src_group <- SACTN %>%
  group_by(src, date) #grouped by source and date

SACTN_date_group <- SACTN %>%
  group_by(date, depth) #grouped by date and depth

SACTN_ungroup <- SACTN_date_group %>%
  ungroup() #ungrouping

#CHAIN FUNCTION
SACTN_depth_mean_2 <- SACTN %>% 
  group_by(depth) %>% #Grouped by depth
  summarise(mean_temp = mean(temp, na.rm = TRUE), #Calculate mean
            count = n()) 

SACTN_30_years <- SACTN %>%
  group_by(site, src) %>% #these will be grouped together
  filter(n() > 360) #360 months = 30 years

SACTN_anom <- SACTN %>% #anomaly for each site
  group_by(site, src) %>%
  mutate(anom = temp - mean(temp, na.rm = T)) %>%
  select(site:date, anom, depth, type) %>%
  ungroup()

#VECTOR - PRACTICE THIS
selected_sites <- c("Paternoster", "Oudekraal") #making a vector
#creating a set of sites
#this is contrasted to this which is one site:
SACTN %>% 
  filter(site == "Port Nolloth") 

SACTN %>%
  filter(site %in% selected_sites) %>% #the site column in those selected sites
  group_by(site, src) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            sd_temp = sd(temp, na.rm = TRUE))

selected_sites <- c("Paternoster", "Oudekraal", "Muizenberg", "Humewood") #making a vector

SACTN %>%
  filter(site %in% selected_sites) %>% #the site column in those selected sites
  group_by(site, src) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            sd_temp = sd(temp, na.rm = TRUE))

SACTN %>%
  filter(site == "Port Nolloth", temp > 10, temp < 15)

SACTN %>%
  filter(site == "Port Nolloth", !(temp <= 10 | temp >= 15))

#MAKING INTO A PLOT
SACTN %>% 
  filter(site %in% c("Bordjies", "Tsitsikamma", "Humewood", "Durban")) %>%
  select(-depth, -type) %>% #Remove these two columns
  mutate(month = month(date), #Creates a column called month
         index = paste(site, src, sep = "/ ")) %>% #Separates the index column
  group_by(index, month) %>% #Group by individual sites and months
  summarise(mean_temp = mean(temp, na.rm = TRUE), #mean temperature
            sd_temp = sd(temp, na.rm = TRUE)) %>% #standard deviation
  ggplot(aes(x = month, y = mean_temp)) + #the beginning of the ggplot, switching from %>% pipe to +
  geom_ribbon(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp),
              fill = "black", alpha = 0.4) + #Creates a ribbon
  geom_line(col = "red", size = 0.3) + # Create lines within ribbon
  facet_wrap(~index) + #Facet by sites
  scale_x_continuous(breaks = seq(2, 12, 4)) + #X axis ticks
  labs(x = "Month", y = "Temperature (°C)") + #Labels
  theme_dark() #Theme

SACTN %>% #renaming
  rename(source = src)

#TRANSMUTE
SACTN_transmute <- SACTN %>%
  transmute(kelvin = temp + 273.15) 
#creates a new variable column but doesn't want to keep the original

SACTN %>%
  group_by(site, src) %>%
  transmute(kelvin = temp + 273.15)

SACTN_n <- SACTN %>%
  group_by(site, src) %>%
  summarise(mean_temp = round(mean(temp, na.rm = T))) %>%
  ungroup() %>%
  select(mean_temp) %>%
  group_by(mean_temp) %>%
  summarise(count = n())
ggplot(data = SACTN_n, aes(x = 1:nrow(SACTN_n), y = mean_temp)) +
  geom_point(aes(size = count)) +
  labs(x = "", y = "Temperature (°C)") +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

SACTN %>%
  group_by(site, src) %>%
  summarise(mean_temp = round(mean(temp, na.rm = T))) %>%
  ungroup() %>%
  ggplot(aes(x = mean_temp)) +
  geom_density(fill = "midnightblue", alpha = 0.6) +
  labs(x = "Temperature (°C)")

#SLICE 
SACTN %>%
  slice(10010:10020) #slices a sequence

SACTN %>%
  slice(c(1,8,19,24,3,400)) #slices specific rows

SACTN %>%
  slice(-(c(1,8,4))) #slices everything except these

SACTN %>%
  slice(-(1:1000)) #slices all rows except this sequence

SACTN %>%
  group_by(site, src) %>%
  summarise(sd_temp = sd(temp, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(sd_temp)) %>%
  slice(1:5) #top 5 variable sites, measured by SD

SACTN %>%
  na.omit() %>% #removes n/a data
  group_by(src) %>%
  summarise(count = n(),
            count_15 = sum(temp > 15)) %>%
  mutate(prop_15 = count_15/count) %>%
  arrange(prop_15) #summarises data

read_csv("data/SACTN_data.csv") %>% #Loads the SACTN Day 1 data from a CSV
  mutate(month = month(date)) %>% #Creates a month abbreviation column
  group_by(site, month) %>% #Group by sites and months
  summarise(mean_temp = mean(temp, na.rm = TRUE), #Calculates mean
            sd_temp = sd(temp, na.rm = TRUE)) %>% #Calculates SD
  ggplot(aes(x = month, y = mean_temp)) + #Begins ggplot and switches from %>% to +
  geom_ribbon(aes(ymin = mean_temp - sd_temp, ymax = mean_temp + sd_temp),
              fill = "black", alpha = 0.5) + #Creates a ribbon
  geom_point(aes(colour = site)) + #Creates dots
  geom_line(aes(colour = site, group = site)) + #Create lines
  labs(x = "", y = "Temperature (°C)", colour = "Site") #Labels