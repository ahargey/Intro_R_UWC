#DAY 4
#Tidier data
#Ayesha Hargey
#3650393
#1 February 2019

#Load libraries 
library(tidyverse) #always good to read up on the libraries
library(lubridate) #a very important library to deal with dates

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
#Or the 1st month from the date column 

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

SACTN %>%
  filter(site == "Humewood", year(date) == 1990, month(date) == 11, day(date) == 1)
#how to filter by multiple date variables like year, month and day

#SELECT
#Select columns individually

try1 <- SACTN %>% #these are all new dataframes
  select(site, src, date, temp) #Only these 4

#Select all columns between two variables (site and temp) like a sequence
try2 <- SACTN %>%
  select(site:temp) #site till temp

#Select all columns except the following
try3 <- SACTN %>%
  select(-date, -depth)

#Select all columns except those in a sequence
#The '-' goes outside of a new set of brackets
#So in between two sets of ()
try4 <- SACTN %>%
  select(-(date:depth))


#CREATING NEW VARIABLES
#MUTATE 
try5 <- SACTN %>%
  mutate(kelvin = temp + 273.15) #new column called 'kelvin'
#the contents of kelvin is what's in the temp column plus the value 

SACTN %>%
  mutate(half_temperature = temp/2) #if you wanted half the temp

#SUMMARISE
#gives you the mean, sd, min, max
#CONTRASTED TO SUMMARY WHICH IS SUMMARY OF DATA
SACTN %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            sd_temp = sd(temp, na.rm = TRUE),
            min_temp = min(temp, na.rm = TRUE),
            max_temp = max(temp, na.rm = TRUE))
#specify dataset used
#have to give a name to the column 
#na.rm eliminates n/a results
#can also just use = T instead of = TRUE



