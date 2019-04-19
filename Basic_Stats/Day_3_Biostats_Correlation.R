#AYESHA HARGEY
#3650393
#16th April 2019
#Basic Statistics - Day 3 - CORRELATIONS 

#Correlations
#Load libraries

library(tidyverse)
library(ggpubr)
library(corrplot)
library(RColorBrewer)

#Load data from csv
ecklonia <- read_csv("data/ecklonia.csv")

#Explore data

#Pearson correlation
#Specify which correlation is done
#Perform correlation analysis on two specific variables
# 
cor.test(x = ecklonia$stipe_length, ecklonia$frond_length, #this is the code always used
         use = "everything", method = "pearson") #can be changed but not normally done

#do a correlation test on the ecklonia dataset on the stipe length column
#and then compare it with the frond length 

#can write out the results in the same format, just type properly 

#a great correlation is closer to 1 

ecklonia_sub <- ecklonia %>% 
  select(-species, -site, -ID)

ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

# Create ordinal data
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), breaks = 3))

# Run test on any variable
cor.test(ecklonia$length, ecklonia$digits)

#KENDALL USED ON NON NORMAL DATA
#EVEN IF CONTINUOUS OR DISCRETE

ecklonia_norm <- ecklonia_sub %>% #new name to data
  gather(key = "variable") %>% #collects it all under one
  group_by(variable) %>%  #group it by variable
  summarise(variable_norm = as.numeric(shapiro.test(value)[2])) #tests for normality
ecklonia_norm

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")
#last value is cor
#0.34 is the cor value
#closer to 1, the stronger the correlation

# Calculate Pearson r beforehand for plotting
r_print <- paste0("r = ", #outcomes in environment as a value
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))
#paste0 saves a value that can then be added as a value

#One correlation
ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) +
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()

#heat map would be a good one to use
corrplot(ecklonia_pearson, method = "circle") 
#the size of the dot is the correlation
#big, dark dot is a strong correlation
#light, small dot is a weak concentration

#HEATMAP
colour = colorRampPalette(brewer.pal(8, "PuOr"))(25)
heatmap(ecklonia_pearson, scale="column",
        Colv = NA, Rowv = NA, col = colour)
