#EXERCISE 2
#R Workshop
#AYESHA HARGEY
#3650393
#5th February 2019

#load libraries
library(tidyverse)
library(lubridate)
library(ggpubr)

#NUMBER ONE

load("data/SACTNmonthly_v4.0.RData") #load dataset

#Rename and remove the old one
SACTN <- SACTNmonthly_v4.0 #renamed for efficency 
remove(SACTNmonthly_v4.0)

#Explore the data
glimpse(SACTN) #overall preview of data, shows every column
head(SACTN, n = 9) #first nine rows
tail(SACTN, n = 9) #last nine rows
nrow(SACTN) #number of row
ncol(SACTN) #number of columns
dim(SACTN) #dimensions
any(is.na(SACTN)) #is there any missing data?
summary(SACTN) #summary of the data according to quartiles and min/max values

#YEARLY MEAN
SACTN_mean_1 <- SACTN %>% #making a new dataframe for the mean data
  filter(src == "KZNSB") %>%  #filtering only for the speciic source
  separate(col = date, into = c("year","month","day"), sep = "-") %>% #separating dates so it can be grouped by year
  group_by(site, year) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE))

SACTN_annual_means <- ggplot(data = SACTN_mean_1, aes(x = year, y = mean_temp)) +
  geom_line(aes(group = site), colour = "seagreen1") + #changed colour of line
  facet_wrap(~site,ncol = 5) + #each site is an individual plot 
  labs(x = "Year", y = "Temperature (C°)") + #labels
  scale_x_discrete(breaks = c("1980", "2000")) + #sets the x scale, since it is discrete it's in inverted commas
  scale_y_continuous(breaks = c(20, 22, 24)) + #sets the y scale, since it is continuous, it isn't in inverted commas
  ggtitle("KZNSB: series of annual means") #title
SACTN_annual_means #final graph projected

#NUMBER TWO  
lam <- read_csv("data/laminaria.csv") #read data from csv

#FILTER FOR FALSEBAY ONLY
lam_fb <- lam %>% #filters by FB only
  filter(region == "FB")

lam_graph_original <- ggplot(data = lam_fb, aes(x = blade_length, y = blade_weight)) +
  geom_line(aes (colour = site)) +
  geom_point(aes (colour = site)) +
  facet_wrap(~site, ncol = 3) + #individual graph for each site
  scale_colour_brewer(palette = "Accent") + #palette used 
  labs(x ="Blade length (cm)", y = "Blade mass (kg)") + #labels
  ggtitle("A crazy graph of some data for False Bay sites") #title
lam_graph_original #graph projected

#When run like that, R gives the following error message:
# ERROR MESSAGE:
# In RColorBrewer::brewer.pal(n, pal) :
#   n too large, allowed maximum for palette Accent is 8
# Returning the palette you asked for with that many colors
# 
# 2: Removed 11 rows containing missing values (geom_point). 
#This means that the palette chosen "Accent" only has 8 colours
#And the current dataset has 9 variables
#Consequently the last site would then be left unfilled
#This is remedied by either creating another colour palette with enough colours
#Or using one of R's colour palette's that have enough colours.


lam_graph_fixed <- ggplot(data = lam_fb, aes(x = blade_length, y = blade_weight)) +
  geom_line(aes (colour = site)) +
  geom_point(aes (colour = site)) +
  facet_wrap(~site, ncol = 3) +
  scale_colour_brewer(palette = "Set1") + #palette changed to one of R's that has enough colours
  labs(x ="Blade length (cm)", y = "Blade mass (kg)") + #all other specifics remain the same
  ggtitle("Fixed graph of data for False Bay sites") #new title
lam_graph_fixed #graph projected

lam_A <- annotate_figure(lam_graph_original, #adding the title A to the plot
                top = text_grob("A", color = "hotpink1", 
                                face = "bold", size = 14))

lam_B <- annotate_figure(lam_graph_fixed, #adding the title B to the plot
                top = text_grob("B", color = "seagreen3", 
                                face = "bold", size = 14))

lam_arrange <- ggarrange(lam_A, lam_B, ncol = 2) #puts both plots on one graph
lam_arrange #final graph projected

#NUMBER THREE
tooth <- datasets::ToothGrowth #loading dataset
??ToothGrowth #information about the dataset

#Explore the data
glimpse(tooth) #overall preview of data, shows every column
head(tooth) #first six rows
tail(tooth) #last six rows
nrow(tooth) #number of row
ncol(tooth) #number of columns
dim(tooth) #dimensions
any(is.na(tooth)) #is there any missing data?
summary(tooth) #summary of the data according to quartiles and min/max values

#In this case, x wants to be treated as a categorial variable instead of a continuous one
#So it must be converted to a factor

tooth_data <- tooth %>% #getting the mean of the data 
  group_by(supp, dose) %>% 
  summarise(ml = mean(len),
            sd_l = sd(len))

tooth_bar <- ggplot(data = tooth_data, aes(x = dose, y = ml, fill = supp)) + 
  geom_bar(stat = "identity", position= "dodge", colour="black", size = 1.5) + #position dodge so it doesn't overlap
  labs(x = "Dose(mg/d)", y = "Tooth length (mm)") + #labels
  geom_errorbar(aes(ymin = ml - sd_l, ymax = ml + sd_l), width=.2, #error bars using mean length +/- standard deviation 
                size = 1.5, position = position_dodge(.9)) #size adjusts line thickness
tooth_bar #final graph projected