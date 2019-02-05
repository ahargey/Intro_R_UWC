#EXERCISE 2
#R Workshop
#AYESHA HARGEY
#3650393
#5th February 2019

#load libraries
library(tidyverse)
library(lubridate)
library(ggpubr)

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
  