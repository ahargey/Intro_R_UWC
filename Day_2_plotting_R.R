# Introduction to Plotting in R using ggplot2
# Day 2
# 30th January 2019
# Ayesha Hargey

# Load libraries
library(tidyverse)

chicks <- datasets::ChickWeight #dataset loaded from R
??ChickWeight #information about the dataset 

#in graphs, '+' works as the pipe
ggplot(data = chicks, aes(x = Time, y = weight)) + #parent line, followed by child
  geom_point() + #scatterplot 
  geom_line(aes(group = Chick))

ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) + #no need to specify data
  geom_point() + #empty closed brackets 
  geom_line(aes(group = Chick))

ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") #creates a smooth line, 'lm' is 'linear model'

ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point(colour = "maroon") +
  geom_smooth(method = "lm")

ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point(aes(size = weight)) +
  geom_smooth(method = "lm") +
  labs(x = "Days", y = "Weight (kg)") +
  ggtitle("Chicks") +
  theme_bw()

#creating own graph
lam <- read.csv("data/laminaria.csv")

ggplot(lam, aes(x = blade_length, y = total_length, colour = site)) +
  geom_point(colour = "maroon")aes(weight)  +
  geom_smooth(method = "lm") +
  labs(x = "Blade Length (m)", y = "Total Length (m)") +
  ggtitle("Length of Seaweed") +
  theme_bw ()

# Faceting in ggplot
library(ggpubr)

ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~Diet, ncol=4)


chicks_2 <- chicks %>%  #new dataframe
  filter(Time == 21) #only data from Day 21

plot_1 <- ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_line(aes(group = Chick)) +
  labs(x = "Days", y = "Weight (mg)") +
  ggtitle("Chick Weight A")
plot_1 #will project the plot after creation

plot_2 <- ggplot(chicks, aes(x = Time, y = weight, colour = Diet)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggtitle ("Chick Weight B")
plot_2 #will project the plot after creation

plot_3 <- ggplot(chicks_2, aes(x = weight)) +
  geom_histogram(aes(fill = Diet), position = "dodge", binwidth = 100) +
  labs(x = "Final Mass (g)", y = "Count")
plot_3 #will project the plot after creation

plot_4 <- ggplot(chicks_2, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet)) +
  labs(x = "Diet", y = "Final Mass (g)")
plot_4 #will project the plot after creation

#plot combined
plot_combined <- ggarrange(plot_1, plot_2, plot_3, plot_4)
plot_combined
#makes one big plot composed of other graphs


#HOMEWORK:
#Any three datasets
#choose three built-in databases, and make two graphs per set
#(facets - line, histogram, box and whisker)
#(colours, lines, size)
#plot with purpose
#Make a hypothesis and create graphs that will answer it
#Calculate mean of one of those columns for each of these datasets
#so three mean calculations in total

#3rd library
library(boot)

urine <- boot::urine #assign a name to get into environment
??urine

urine %>% #because this isn't assigned, you can edit the data with no consequence
  select(-cond, -ph) #this is how to remove a column

ggplot(data = urine, aes(x = osmo, y = ph)) +
  geom_point(aes(colour = cond))

ggplot(data = urine, aes(x = osmo, y = ph)) +
  geom_point(aes(colour = as.factor(r)))

# [A.A]
# Neat script
# A bit more comments will only make studying easier in future
# Good work
