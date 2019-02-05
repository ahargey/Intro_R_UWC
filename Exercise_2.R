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
  geom_line(aes(group = site)) +
  facet_wrap(~site, ncol = 5) +
  labs(x = "Year", y = "Temperature (C°)") +
  scale_x_discrete(breaks = c("1980", "2000")) +
  scale_y_continuous(breaks = c("20", "22", "24"))



SACTN_mean <- SACTN %>%
  mutate(date = ymd(str_remove(date, "X")))


  gather(date, temp, -site, -src, -depth, -type) %>%
  mutate(date = ymd(str_remove(date, "X"))) %>%
  mutate(year = year(date)) %>%
  group_by(site, temp, year) %>%
  summarise_at(vars(temp), mean, na.rm = TRUE) %>%
  spread(year, temp)
lubridate

df %>%
  mutate(date = floor_date(date)) %>%
  group_by(date) %>%
  summarize(mean_X1 = mean(X1))