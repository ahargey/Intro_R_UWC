# Day 1 
# Laminaria dataset exploring and learning
# Ayesha Hargey
# 29th January 2019


#Loading libraries 

library(tidyverse)
lam <- read.csv("data/laminaria.csv")
head(lam) #shows first six rows
tail(lam) #shows last six rows
head(lam, n = 3)
tail(lam, n = 3)

lam_select <- lam %>% 
  select(site, total_length) %>% 
  slice(54:80)

lam_kom <- lam %>%
  filter(site == "Kommetjie")

# In the laminaria dataset, select only site and blade_length column 
# filter only for Sea Point

lam_sea <- lam %>% 
  select(site, blade_length) %>% 
  filter(site == "Sea Point")

lam %>% 
  filter(total_length == max(total_length))

summary(lam)
dim(lam) #stands for dimensions

lam %>% 
  summarise(avrg_bl = mean(blade_length),
            med_bl = median(blade_length),
            sd_bl = sd(blade_length)) #"avrg_bl" is the new column created

lam %>% #standard error
  group_by(site) %>% 
  summarise(var_bl = var(blade_length),
            n = n()) %>% 
  mutate(se = sqrt(var_bl/n)) #creates a new column

# select lam dataset, group it by site, summarise by
# calculating the blade length of all the sites
# and the variance in it for each group of sites 

lam_2 <- lam %>% 
  select(-blade_thickness, -blade_length) #removes a column

lam_count <- lam %>% #select laminaria dataset and then
  select(stipe_mass) %>% #select stipe mass
  na.omit %>% #gets rid of N/A results
  summarise(n = n())

lam %>% #select laminaria dataset and then
  select(blade_length) %>% #select stipe mass
  summarise(n = n())

lam %>% #select laminaria dataset and then
  select(blade_length) %>% #select stipe mass
  na.omit %>% #gets rid of N/A results
  summarise(n = n())

ggplot(data = lam, aes(x = stipe_mass, y = stipe_length)) +
  geom_point(shape = 21, colour = "magenta", fill = "black") +
  labs(x = "Stipe mass (kg)", y = "Stipe length (cm)") #graph

# Exercise: 
  
# 1. Create a new data frame from the `laminaria` dataset that meets the following criteria: contains only the `site` column and a new column called `total_length_half` containing values that are half of the `total_length`. In this `total_length_half` column, there are no `NA`s and all values are less than 100.
# **Hint**: think about how the commands should be ordered to produce this data frame!
#   
# 2. Use `group_by()` and `summarize()` to find the mean, min, and max blade_length for each site. Also add the number of observations (hint: see `?n`).
# 
# 3. What was the heaviest stipe measured in each site? Return the columns `site`, `region`, and `stipe_length`.

#should only have site and length
#2.

lam_exercise2 <- lam %>%
  group_by(site) %>% 
  summarise(mean_bl = mean(blade_length),
            min_bl = min(blade_length),
            max_bl = max(blade_length),
            n = n())

#3.
lam_exercise3.1 <- lam %>% 
  group_by(site, region, stipe_length) %>%
  summarise(heaviest_stipe = max(stipe_mass)) %>%
  na.omit


#1 data frame from the `laminaria` that contains site` column and 
#a new column called `total_length_half` 
#containing values that are half of the `total_length`. 
#In this `total_length_half` column, there are no `NA`s and all values are less than 100.

lam_exercise1 <- lam %>%
  group_by(site, total_length)a
  # half two

  
  

  