#RECREATING GRAPH
#R Homework
#AYESHA HARGEY
#3650393
#7th February 2019

#Load libraries
library(tidyverse)
library(ggpubr)
library(scales)
library(ggsn)

#Load file from .csv
library(readr)
egg <- read_delim("data/YearlyEggBreadths.csv", #delimated by semi-colon
                                ";", escape_double = FALSE, trim_ws = TRUE)
egg

#Explore data
glimpse(egg) #overall preview of data, shows every column
head(egg) #first six rows
tail(egg) #last six rows
nrow(egg) #number of row
ncol(egg) #number of columns
dim(egg) #the dimensions of the data i.e how many rows and columns 
any(is.na(egg)) #is there any missing data?
summary(egg) #summary of the data according to quartiles and min/max values

AVERAGES
egg_slice <- egg %>% 
  slice(-(13:16)) #getting rid of meaningless columns 

egg_averages <- egg_slice %>% #THIS IS CORRECT AS IS 
  group_by(Months) %>% 
  summarise(average_mean = mean(AveragesMinBreadth1, AveragesMinBreadth2))


egg_average_both <- egg_slice %>% #TRYING SOMETHING NEW
  group_by(Months) %>% 
  summarise(average1_mean = mean(AveragesMinBreadth1), 
            average2_mean = mean(AveragesMinBreadth2))


egg_slice %>% 
  mean(., AveragesMinBreadth1)
mean(StdDevMinBreadth1)

library(dplyr)
test_egg <- egg_slice %>% mutate(Averages=(AveragesMinBreadth1+AveragesMinBreadth2)/2, Adjusted=(StdDevMinBreadth1+StdDevMinBreadth2)/2)
test_egg

  summarise(adjusted_average_mean = mean(StdDevMinBreadth1, StdDevMinBreadth2))
    

egg_slice %>%
  group_by(Months) %>%
  mutate(Mean = rowMeans(c("AveragesMinBreadth1":"AveragesMinBreadth2")))
  
  
egg_slice %>% 
  rowMeans(AveragesMinBreadth1, AveragesMinBreadth2)  
  
  mean



    average_mean = mean((AveragesMinBreadth1, AveragesMinBreadth2)/2),
            adjusted_average_mean = mean(StdDevMinBreadth1, StdDevMinBreadth2))
           
            
            
             sd_l = sd(len))
  
  
  mutate(mean = rowMeans(egg_slice[c("AveragesMinBreadth1", "AveragesMinBreadth2")], na.rm=TRUE))
    
  
egg_slice %>%
group_by(Months) %>%   

  test <- rowMeans(egg_slice[c("AveragesMinBreadth1", "AveragesMinBreadth2")], na.rm=TRUE)

  summarise(average = mean()
            adjusted = 

ADJUSTED

egg_slice$Months <- as.numeric(as.character(egg_slice$Months))

            
ggplot(egg_slice, aes(x = Months, y = AveragesMinBreadth1)) +
  geom_line(aes(colour = Months, group = AveragesMinBreadth1)) 
 

test <- egg_slice
test$month <- factor(test$month)
ggplot(data=test, aes(x=Months, y=AveragesMinBreadth1)) +
  geom_line(group = Months) +

str(egg_slice) 
egg_slice$Months <- as.numeric(as.character(egg_slice$Months))