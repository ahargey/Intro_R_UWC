#Biostats Code Cheat Sheet
#Don't forget metadata

#Load libraries
library(fitdistrplus)
library(tidyverse) #loaded second to prevent masking of the select function
library(ggpubr)
library(RColorBrewer) #for palettes
library(ggthemes)
library(logspline)
library(e1071)
library(corrplot)
library(reshape2)


#Data exploring
#Explore the data
glimpse(cuckoos) #overall preview of data, shows every column
head(cuckoos) #first six rows
tail(cuckoos) #last six rows
nrow(cuckoos) #number of row
ncol(cuckoos) #number of columns
any(is.na(cuckoos)) #is there any missing data?
summary(cuckoos) #gives a summary of the mean, median, quartiles and min/max values

#Normality

skewness(cuckoos$length) #skewness

kurtosis(cuckoos$length) %>% 
  round(0)
#kurtosis is 0 which means data is normal

#Transformation
chicks_transformed <-  chicks %>% #mutate creates new column
  mutate(log = log(weight))

ggqqplot(cuckoos, x = "length") #checks for normality visually
descdist(cuckoos$length, discrete = FALSE, boot = 100) #data is normal

cuckoos %>% #test for homoscedasticity
  group_by(species) %>%
  summarise(length_var = var(length),
            breadth_var = var(breadth)) 
#variance is not 2-4 times greater
#data is homoscedastic 

shapiro.test(chicks$weight) #normality test
#When p >= 0.05 we may assume that
#the data are normally distributed
#if p < 0.05 then the data are not normally distrubted.