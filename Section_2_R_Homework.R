#SECTION 2
#R Homework
#AYESHA HARGEY
#3650393
#3rd eckruary 2019

# Section 2: 
# Make use of the ecklonia.csv dataset:
# Explore the data (Hint* head, tail, glimpse functions)
# Demonstrate the dimensions of the dataset (dim)
# Create three graphs; bargraph, line graph and boxplot: Write hypothesis for each of the graphs and answer these hypotheses
# Make use of the ggarrange function and arrange these three graphs created above into 1 plot
# All graphs must have labels as well as titles !and themes!
# Calculate the mean,max,min,median and variance for the stipe_length, stipe_diameter for each of the sites (Hint* group_by site)
# Calculate standard error !se!
# #eck %>% #standard error
# group_by(site) %>%
#   summarise(var_bl = var(blade_length),
#             n = n()) %>%
#   mutate(se = sqrt(var_bl/n)) #creates a new column

# Determine the min and maximum frond length and stipe length
# Determine the overall summary of the dataset !summary(wholedatasetname)

#Load libraries
library(tidyverse)
library(ggpubr)

#Load data from csv
eck <- read_csv("data/ecklonia.csv")

#Explore the data
glimpse(eck) #overall preview of data, shows every column
head(eck, n = 9) #first nine rows
tail(eck, n = 9) #last nine rows
nrow(eck) #number of row
ncol(eck) #number of columns
any(is.na(eck)) #is there any missing data?
summary(eck) #summary of the data according to quartiles and min/max values

#Dimensions
dim(eck) 
#26 rows and 12 columns

#BAR GRAPH
#HYPOTHESIS: there are more digits in boulders beach than batsata beach
eck_bar <- 
  
  ggplot(eck, aes(x = site)) +
  geom_bar()


(aes(x = digits, colour = stipe_length))

           