#AYESHA HARGEY
#3650393
#9th April 2019
#Basic Statistics - Day 1 

#Load libraries
library(tidyverse)
#Load chickweight data as tibble
chicks <- as_tibble(ChickWeight)
head(chicks) #first 6 rows
head(chicks, n = 20) #first 20 rows
tail(chicks, n = 10) #last 10 rows
colnames(chicks) #column names
summary(chicks) #summary of data (min, max, quantiles)
dim(chicks) #dimensions

names <- colnames(chicks)
names 
names[4] #shows the fourth category of column names

ncol(chicks) #number of columns
nrow(chicks) #number of rows
glimpse(chicks) #first few data points

round(mean(chicks$weight),1) #dollar sign separates the dataframe from the column

chicks %>% 
  filter(Time == 0) %>% #first day
  group_by(Diet) %>%  #selecting only the Diet column
  summarise(mean_wt = mean(weight), #new column called mean_wt
            sd_wt = sd(weight)) %>% #new column called sd_wt
  ungroup()

chicks %>% 
  summarise(length = n()) #total sample size, same as nrow

#OR

length(chicks$weight) #total sample size, same as nrow, same as above

#the mean for a customized set of data
numbers <- c(2, 712, 666, 99, 24)
sum(numbers / length(numbers))
mean(numbers)

chicks %>% 
  summarise(mean_wt = mean(weight)) #weight of a typical chicken

round(mean(chicks$weight),1) #rounding up the value, with the mean function nested

missing <- c(2, 4, 6, 8, NA, 10)
mean(missing) #cannot give an answer
mean(missing, na.rm = TRUE) #removes the NA function

#ALTERNATIVELY
mean(na.omit(missing))

numbers2 <- c(5, 2, 6, 13, 1)
mean(numbers2) 
median(numbers2)

numbers3 <- c(5, 2, 6, 5000, 1)
mean(numbers3) #mean is highly influenced by huge numbers like 5000
median(numbers2) #median is not

quantile(chicks$weight)

chicks %>% 
  summarise(min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25), #quartile 1
            med_wt = median(weight), #quartile 2 // median
            qrt3_wt = quantile(weight, p = 0.75), #quartile 3
            max_wt = max(weight))

range(chicks$weight) #has min and max combined
min(chicks$weight) #just min
max(chicks$weight) #just max

#in class exercise
#all chickens younger than 10 days earlier (every day less than ten)
#each of the 4 different diets
#produce a summary

summary(chicks$weight)

day1_chicks <- chicks %>% #shows stats for the combined 10 days 
  filter(Time <= 10) %>% #up to and including ten day
  group_by(Diet) %>% 
  summarise(min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25), #quartile 1
            med_wt = median(weight), #quartile 2 // median
            qrt3_wt = quantile(weight, p = 0.75), #quartile 3
            max_wt = max(weight))

individual_chicks <- chicks %>% #shows stats for each individual day
  filter(Time <= 10) %>% 
  group_by(Diet, Time) %>% 
  summarise(min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25), #quartile 1
            med_wt = median(weight), #quartile 2 // median
            qrt3_wt = quantile(weight, p = 0.75), #quartile 3
            max_wt = max(weight))

#Load libraries
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(ggthemes)

#using the iris dataset 
iris.cnt <- iris %>%
  count(Species) %>% #makes column n
  mutate(prop = n / sum(n)) #makes column with the proportion of species
iris.cnt

plt1 <- ggplot(data = iris.cnt, aes(x = "", y = n, fill = Species)) + #y is count
  geom_bar(width = 1, stat = "identity") + #stat identity, plot for height 
  labs(title = "Stacked bar graph", subtitle = "cumulative sum", #labels
       x = NULL, y = "Count") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()
plt1

plt2 <- ggplot(data = iris.cnt, aes(x = "", y = prop, fill = Species)) +
  geom_bar(width = 1, stat = "identity") +
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "relative proportions",
       x = NULL, y = "Proportion") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()
plt2

test1 <- ggplot(data = iris.cnt, aes(x = "", y = prop, fill = Species)) + #y is count
  geom_bar(width = 1, stat = "identity") + #stat identity, plot for height
  scale_y_continuous(breaks = c(0.00, 0.33, 0.66, 1.00)) +
  labs(title = "Stacked bar graph", subtitle = "cumulative sum", #labels
       x = NULL, y = "Count") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()
test1

plt3 <- test1 + coord_polar("y", start = 0) +
  labs(title = "Friends don't let...", subtitle = "...friends make pie charts",
       x = NULL, y = NULL) +
  scale_fill_brewer(palette = "Purples") +
  theme_minimal()
plt3

plt4 <- ggplot(data = iris, aes(x = Species, fill = Species)) +
  geom_bar(show.legend = FALSE) +
  labs(title = "Side-by-side bars", subtitle = "n per species", y = "Count") +
  theme_pubclean() + scale_color_few() +
  scale_fill_few()
plt4

ggarrange(plt1, plt2, plt3, plt4, nrow = 2, ncol = 2, labels = "AUTO")

#Old Faithful Dataset
hist1 <- ggplot(data = faithful, aes(x = eruptions)) + #each bin has same length
  geom_histogram(colour = "black", fill = "salmon", alpha = 0.6) +
  labs(title = "Old Faithful data", #alpha is transparency
       subtitle = "A vanilla frequency histogram",
       x = "Eruption duration (min)",
       y = "Count") + theme_pubclean()
hist1

hist2 <- ggplot(data = faithful, aes(x = eruptions)) +
  geom_histogram(aes(y = ..density..), #density counts proportion // dots have to be there
                 position = 'identity', binwidth = 1,
                 colour = "black", fill = "salmon", alpha = 0.6) +
  labs(title = "Old Faithful data",
       subtitle = "Relative frequency histogram",
       x = "Eruption duration (min)",
       y = "Count") + theme_pubclean()
hist2

hist3 <- ggplot(data = faithful, aes(x = eruptions)) +
  geom_histogram(aes(y = 0.5 * ..density..),
                 position = 'identity', binwidth = 0.5, #fiddle with binwidth
                 colour = "black", fill = "salmon", alpha = 0.6) +
  labs(title = "Old Faithful data",
       subtitle = "Relative frequency histogram",
       x = "Eruption duration (min)",
       y = "Relative contribution") + theme_pubclean()
hist3

hist4 <- ggplot(data = faithful, aes(x = eruptions)) + 
  stat_ecdf() +
  labs(title = "Old Faithful data",
       subtitle = "ECDF",
       x = "Eruption duration (min)",
       y = "Relative contribution") + theme_pubclean()

ggarrange(hist1, hist2, hist3, hist4, ncol = 2, nrow = 2, labels = "AUTO")

#CONTINUOUS DATA WITH IRIS

#data is converted into a form that is 'long' (tidy)
iris.long <- iris %>% 
  gather(key = "variable", value = "size", -Species)

ggplot(data = iris.long, aes(x = size)) +
  geom_histogram(position = "dodge", #individual columns
                 colour = NA, bins = 20,
                 aes(fill = Species)) +
  facet_wrap(~variable) + #separates 
  labs(title = "Iris data",
       subtitle = "Grouped frequency histogram",
       x = "Size (mm)",
       y = "Count") +
  theme_pubclean()

#box plot
boxplt1 <- ggplot(data = iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot(show.legend = FALSE, notch = FALSE) + theme_pubclean() +
  labs(y = "Sepal length (mm)") +
  theme(axis.text.x = element_text(face = "italic")) #species name in italics
boxplt1

boxplt2 <- ggplot(data = iris.long, aes(x = Species, y = size)) +
  geom_boxplot(fill = "red", alpha = 0.4, notch = TRUE) +
  geom_jitter(width = 0.1, shape = 21, colour = "blue", fill = NA, alpha = 0.2) +
  facet_wrap(~variable, nrow = 1) +
  labs(y = "Size (mm)") + theme_pubclean() +
  theme(axis.text.x = element_text(face = "italic")) +
  theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm"))
boxplt2

#SCATTER PLOT
scatterplt1 <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point() +
  labs(x = "Petal length (mm)", y = "Petal width (mm)") +
  theme(legend.position = c(0.18, 0.85)) +
  scale_color_fivethirtyeight() +
  scale_fill_fivethirtyeight() + 
  theme_pubclean()
scatterplt1

scatterplt2 <- ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point(show.legend = FALSE) +
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE) + #regression line
  scale_color_fivethirtyeight() +
  scale_fill_fivethirtyeight() +
  labs(x = "Petal length (mm)", y = "Petal width (mm)") + 
  theme_pubclean()
scatterplt2

ggarrange(scatterplt1, scatterplt2, ncol = 2, nrow = 1, labels = "AUTO") #puts both graphs together


# Perfect script
# Neat comments
# tried different things
# Nicely done
