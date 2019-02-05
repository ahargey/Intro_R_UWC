#SECTION 4
#R Homework
#AYESHA HARGEY
#3650393
#5th February 2019

#Load libraries
library(tidyverse)
library(ggpubr)

#IRIS DATASET
iris <- datasets::iris #assigning the dataset to the environment
??iris #information about the dataset

#Explore the data
glimpse(iris) #overall preview of data, shows every column
head(iris) #first nine rows
tail(iris) #last nine rows
nrow(iris) #number of row
ncol(iris) #number of columns
any(is.na(iris)) #is there any missing data?
summary(iris) #summary of the data according to quartiles and min/max values

iris_SD <- iris %>% 
  group_by(Species) %>% #grouping by species
  select (Sepal.Length, Sepal.Width) %>% 
  summarise(mean_length = mean(Sepal.Length),  
            mean_width = mean(Sepal.Width),
            var_sepal_length = var(Sepal.Length), #calculating the variance
            var_sepal_width = var(Sepal.Width),
            n = n()) %>% 
  mutate(se_sepal_length = sqrt(var_sepal_length/n), #calculating standard error
         se_sepal_width = sqrt(var_sepal_width/n))

#VOLCANO PLOT
iris_volcano <- ggplot(data=iris, aes(x = Petal.Length)) + 
  stat_density(aes(ymax = ..density..,  ymin = -..density.., #volcano plot emphasizes the density
                       fill = Species, color = Species), 
                   geom = "ribbon", position = "identity") +
  facet_wrap(~Species) + coord_flip() + #flipping the co-ordinates for a more viewable graph
  labs(x = "Petal Length", y = "Density") + #axis labels
  ggtitle("Volcano Plot of Petal Length in 3 Species of Iris") + #titles
  theme_bw () +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, colour = "black", size=12),
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.background = element_rect(fill = "#f0eae8"),
        plot.title = element_text(size=16, face="bold", hjust=0.5))
iris_volcano

#URINE DATASET
library(boot) #for the urine dataset 

urine <- boot::urine #assigning the dataset to the environment
??urine #information about the dataset

#Explore the data
glimpse(urine) #overall preview of data, shows every column
head(urine) #first nine rows
tail(urine) #last nine rows
nrow(urine) #number of row
ncol(urine) #number of columns
any(is.na(urine)) #is there any missing data?
summary(urine) #summary of the data according to quartiles and min/max values

urine_round <- urine %>%
  group_by(round(ph)) %>% #rounding off the pH to a whole number
  select (-ph) %>% #removing the original column
  summarise(mean_osmo = mean(osmo, na.rm = TRUE), #removing na data
            mean_urea = mean(urea, na.rm = TRUE)) #removing na data

#DENSITY PLOT:
urine_densityplot <- ggplot(urine, aes(x = urea)) +
  geom_density(aes(fill = "maroon")) + #fill of the density plot 
  labs(x = "Urea", y = "Density") +
  ggtitle("Density plot of urea concentration in urine") + 
  theme_bw () +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, colour = "black", size=12),
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.background = element_rect(fill = "#f0eae8"),
        plot.title = element_text(size=16, face="bold", hjust=0.5))
urine_densityplot #shows the distribution of the variable urea


