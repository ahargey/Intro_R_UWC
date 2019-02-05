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
  group_by(Species) %>%
  select (Sepal.Length, Sepal.Width) %>% 
  summarise(mean_length = mean(Sepal.Length),  
            mean_width = mean(Sepal.Width),
            var_sepal_length = var(Sepal.Length),
            var_sepal_width = var(Sepal.Width),
            n = n()) %>% 
  mutate(se_sepal_length = sqrt(var_sepal_length/n),
         se_sepal_width = sqrt(var_sepal_width/n))

#RIBBON
iris_ribbon <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_ribbon(aes(ymin = mean(Sepal.Length) - 1.5, ymax = mean(Sepal.Length) + 1.5), fill = "black", alpha = 0.4) +
  geom_point(aes(colour = Species)) +
  geom_line(aes(colour = Species, group = Species)) +
  scale_colour_manual(values = c("lightpink1", "salmon1", "tomato4")) +
  labs(x = "Sepal Length (mm)", y = "Petal Length (mm)") +
  ggtitle("Sepal and Petal Length Across Three Species of Iris") + #title
  theme_bw () +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, colour = "black", size=12),
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.background = element_rect(fill = "#f0eae8"),
        plot.title = element_text(size=16, face="bold", hjust=0.5))
iris_ribbon

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
  group_by(round(ph)) %>% 
  select (-ph) %>%
  summarise(mean_osmo = mean(osmo, na.rm = TRUE),
            mean_urea = mean(urea, na.rm = TRUE)) 

#DENSITY PLOT:
urine_densityplot <- ggplot(urine, aes(x = urea)) +
  geom_density(aes(fill = "maroon")) +
  labs(x = "Urea", y = "Density") +
  ggtitle("Density plot of urea concentration in urine") + 
  theme_bw () +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, colour = "black", size=12),
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.background = element_rect(fill = "#f0eae8"),
        plot.title = element_text(size=16, face="bold", hjust=0.5))
urine_densityplot

