Ayesha Hargey
3650393
R COURSE PRACTISE

#Load libraries
library(tidyverse)
??ChickWeight
chicks <- as_tibble(ChickWeight)
head(chicks) #first rows of data
tail(chicks, n = 2)  #only  first two
colnames(chicks) #column names 
summary(chicks)
chicks %>%
  summarise(length = n())
length(chicks$weight)

testdata <- c(2,5,7,3,10)
mean(testdata)

HMM <- chicks %>%
  summarise(mean_wt = mean(weight))

mean(chicks$weight)

library(e1071)
skewness(faithful$eruptions)
