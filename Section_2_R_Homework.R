#SECTION 2
#R Homework
#AYESHA HARGEY
#3650393
#5th February 2019

# Section 2: 
# Make use of the ecklonia.csv dataset:
# Explore the data (Hint* head, tail, glimpse functions)
# Demonstrate the dimensions of the dataset (dim)
# Create three graphs; bargraph, line graph and boxplot: Write hypothesis for each of the graphs and answer these hypotheses
# Make use of the ggarrange function and arrange these three graphs created above into 1 plot
# All graphs must have labels as well as titles !and themes!
# Calculate the mean,max,min,median and variance for the stipe_length, stipe_diameter for each of the sites (Hint* group_by site)
# Calculate standard error !se!
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
#HYPOTHESIS: Frond length differs from stipe length dependent on site 
eck_bar <- ggplot(eck, aes(x = stipe_length, y = frond_length)) +
  geom_bar(stat = "identity", aes(fill = site), width = 3, position = "dodge") +
  facet_wrap(~site, ncol=2) + #2 columns when faceted
  labs(x = "Stipe Length (mm)", y = "Frond Length (mm)") +
  ggtitle("The relationship between stipe length and frond length") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 40, hjust = 1, colour = "black", size=12),
          axis.text.y = element_text(hjust = 1, colour = "black", size=12),
          plot.title = element_text(size=14, face="bold", hjust=0.5))
eck_bar
  
  
#CONCLUSION: The site Batsata Rock has much longer fronds and stipe lengths than seen at Boulders Beach.
#This could be due to a number of factors such as the amount of pollution and nutrients in the water.
  
#HYPOTHESIS: The epiphyte length of a species of Ecklonia is strongly related to the stipe length mostly, and this varies by site
eck_line <- ggplot(eck, aes(x = stipe_length, y = epiphyte_length, colour = site)) +
  geom_point() +
  scale_colour_manual(values = c("red4", "royalblue4")) + # How to use custom palette
  labs(colour = "Site of Sample") +
  geom_smooth(method = "lm") +
  labs(x = "Stipe Length (mm)", y = "Epiphyte Length (mm)") + #labels
  ggtitle("Relationship between stipe length and epiphyte length between sites") + #title
  theme_bw () +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, colour = "black", size=12),
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.title = element_text(size=14, face="bold", hjust=0.5))
eck_line
#CONCLUSION: While the species of Ecklonia found in Boulders Beach show a sharper relationship between the height of an epiphyte in relation to the stipe
#It can be seen that Batsata Rock Ecklonia is bigger on average.
#This may be due to the environmental conditions present.

  
#BOXPLOT    
#HYPOTHESIS: Ecklonia found in the Batsata Rock site is heavier than found in Boulders Beach 

eck_box <- ggplot(eck, aes(x = frond_mass, y = stipe_mass)) +
  geom_boxplot(aes(fill = site)) +
  labs(x = "Frond Mass (g)", y = "Stipe Mass (g)") +
  ggtitle("Relationship between stipe mass and frond mass between sites") + #title
  theme_bw () +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, colour = "black", size=12),
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.title = element_text(size=14, face="bold", hjust=0.5))
eck_box #will project the plot after creation     

#CONCLUSION: While there is some overlap between the two sites, on average Batsata Rock has heavier species of Ecklonia

final_plot <- ggarrange(eck_bar, eck_line, eck_box) +
  ggtitle("Three Graphs Displaying the Ecklonia Dataset") + #title
  theme(plot.title = element_text(size=18, colour = "slategray", face="bold", hjust=0.5))
  

final_plot

eck %>%
  group_by(site) %>% 
  summarise(mean_sl = mean(stipe_length), #did not need to pipe because same function
            min_sl = min(stipe_length), #minimum
            max_sl = max(stipe_length), #maximum
            median_sl = median(stipe_length), #median
            var_sl = var(stipe_length)) #variance

eck %>%
  group_by(site) %>% 
  summarise(mean_dm = mean(stipe_diameter), #did not need to pipe because same function
            min_dm = min(stipe_diameter), #minimum
            max_dm = max(stipe_diameter), #maximum
            median_dm = median(stipe_diameter), #median
            var_dm = var(stipe_diameter)) #variance

eck %>% #standard error of stipe length
  group_by(site) %>% 
  summarise(var_sl = var(stipe_length),
  n = n()) %>%
  mutate(se = sqrt(var_sl/n)) #creates a new column

eck %>% #standard error of stipe diameter
  group_by(site) %>%
  summarise(var_sl = var(stipe_diameter),
            n = n()) %>%
  mutate(se = sqrt(var_sl/n)) #creates a new column

eck %>%
  summarise(min_fl = min(frond_length),
            max_fl = max(frond_length),
            min_sl = min(stipe_length),
            max_sl = max(stipe_length))

summary(eck)
