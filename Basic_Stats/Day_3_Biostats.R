#AYESHA HARGEY
#3650393
#16th April 2019
#Basic Statistics - Day 3 

#Linear regression
#Load libraries
library(tidyverse)
library(ggplot2)

#Linear Model
#eruptions as a function of time // of the dataset 'faithful'
eruption.lm <- lm(eruptions ~ waiting, data = faithful) #naming the linear model
summary(eruption.lm) #summary of linear model

str(eruption.lm)

#null hypothesis is rejected
#there is a relationship between waiting time and eruption time

#create a graph for the data faithful
#x-axis: waiting
#y-axis: eruption
#properly labelled

faithful <- faithful

eruption_plot <- ggplot(faithful, aes(x = waiting, y = eruptions)) +
  geom_point() +
  geom_smooth(method = "lm", aes(colour = "Salmon")) +
  labs(x = "Waiting Time (minutes)", y = "Eruptions (minutes)") +
  ggtitle("The Relationship between Eruption Time and Waiting Time of Old Faithful") +
  theme_bw () +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, colour = "black", size=12),
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.background = element_rect(fill = "#f0eae8"),
        plot.title = element_text(size=16, face="bold", hjust=0.5)) +
  geom_label(aes(x = 40, y = 4.5), hjust = 0, #adding the box with all the information
             label = paste("Adj R2 = ",signif(summary(eruption.lm)$adj.r.squared, 5),
                           "\nIntercept =",signif(eruption.lm$coef[[1]],5 ),
                           " \nSlope =",signif(eruption.lm$coef[[2]], 5),
                           " \nP =",signif(summary(eruption.lm)$coef[2,4], 5)))
eruption_plot


  
  
      