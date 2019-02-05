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

load("data/SACTNmonthly_v4.0.RData")

#Rename and remove the old one
SACTN <- SACTNmonthly_v4.0
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
SACTN_mean_1 <- SACTN %>%
  filter(src == "KZNSB") %>% 
  separate(col = date, into = c("year","month","day"), sep = "-") %>% 
  group_by(site, year) %>% 
  summarise(mean_temp = mean(temp, na.rm = TRUE))

ggplot(data = SACTN_mean_1, aes(x = year, y = mean_temp)) +
  geom_line(aes(group = site), colour = "seagreen1") +
  facet_wrap(~site,ncol = 5) +
  labs(x = "Year", y = "Temperature (C°)") +
  scale_x_discrete(breaks = c("1980", "2000")) +
  scale_y_continuous(breaks = c(20, 22, 24)) +
  ggtitle("KZNSB: series of annual means")

#NUMBER TWO  
lam <- read_csv("data/laminaria.csv")

#FILTER FOR FALSEBAY ONLY
lam_fb <- lam %>% 
  filter(region == "FB")

lam_graph_original <- ggplot(data = lam_fb, aes(x = blade_length, y = blade_weight)) +
  geom_line(aes (colour = site)) +
  geom_point(aes (colour = site)) +
  facet_wrap(~site, ncol = 3) +
  scale_colour_brewer(palette = "Accent") + 
  labs(x ="Blade length (cm)", y = "Blade mass (kg)") +
  ggtitle("A crazy graph of some data for False Bay sites")
lam_graph_original

# ERROR MESSAGE:
# In RColorBrewer::brewer.pal(n, pal) :
#   n too large, allowed maximum for palette Accent is 8
# Returning the palette you asked for with that many colors
# 
# 2: Removed 11 rows containing missing values (geom_point). 

lam_graph_fixed <- ggplot(data = lam_fb, aes(x = blade_length, y = blade_weight)) +
  geom_line(aes (colour = site)) +
  geom_point(aes (colour = site)) +
  facet_wrap(~site, ncol = 3) +
  scale_colour_brewer(palette = "Set1") +
  labs(x ="Blade length (cm)", y = "Blade mass (kg)") +
  ggtitle("Fixed graph of data for False Bay sites")
lam_graph_fixed

lam_A <- annotate_figure(lam_graph_original, 
                top = text_grob("A", color = "hotpink1", 
                                face = "bold", size = 14))

lam_B <- annotate_figure(lam_graph_original, 
                top = text_grob("B", color = "seagreen3", 
                                face = "bold", size = 14))

lam_arrange <- ggarrange(lam_A, lam_B, ncol = 2)
lam_arrange

#NUMBER THREE
tooth <- datasets::ToothGrowth
??ToothGrowth

#Explore the data
glimpse(tooth) #overall preview of data, shows every column
head(tooth) #first six rows
tail(tooth) #last six rows
nrow(tooth) #number of row
ncol(tooth) #number of columns
dim(tooth) #dimensions
any(is.na(tooth)) #is there any missing data?
summary(tooth) #summary of the data according to quartiles and min/max values

tooth_mean <- aggregate(len ~ dose + supp, data = tooth, mean)
tooth_mean

tooth_data <- tooth %>% 
  group_by(supp, dose) %>% 
  summarise(ml = mean(len),
            sd_l = sd(len))

ggplot(data = tooth_data, aes(x = dose, y = ml, fill = supp)) +
  geom_bar(stat = "identity", position= "dodge", colour="black") +
  labs(x = "Dose(mg/d)", y = "Tooth length (mm)") +
  geom_errorbar(aes(ymin = ml - sd_l, ymax = ml + sd_l), width=.2,
                position = position_dodge(.9)) 
#FINE UP TILL HERE
  
    
    # Use datn2 from above
    ggplot(data=datn2, aes(x=dose, y=length, fill=supp)) +
    geom_bar(stat="identity", position=position_dodge())
  
  # Use the original data frame, but put factor() directly in the plot specification
  ggplot(data=datn, aes(x=factor(dose), y=length, fill=supp)) +
    geom_bar(stat="identity", position=position_dodge())
    
    