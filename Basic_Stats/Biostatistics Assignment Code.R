#AYESHA HARGEY
#Biostatistics Assignment 
#13 May 2019 

#Load libraries
library(fitdistrplus)
library(tidyverse) #loaded second to prevent masking of the select function
library(ggpubr)
library(RColorBrewer)
library(ggthemes)
library(logspline)
library(e1071)
library(corrplot)

#Dataset chosen
library(DAAG) #datasets and functions used in the book
              #"Data Analysis and Graphics Using R"
cuckoos <- DAAG::cuckoos #naming and selecting the dataset

#Explore the data
iris <- datasets::iris #DELETE THIS

glimpse(cuckoos) #overall preview of data, shows every column
head(cuckoos) #first six rows
tail(cuckoos) #last six rows
nrow(cuckoos) #number of row
ncol(cuckoos) #number of columns
any(is.na(cuckoos)) #is there any missing data?
summary(cuckoos) #gives a summary of the mean, median, quartiles and min/max values

skewness(cuckoos$length) #skewness
#left-skewed / negative skewness
kurtosis(cuckoos$length) %>% 
  round(0)
#kurtosis is 0 which means data is normal

skewness(cuckoos$breadth) #skewness
#left-skewed / negative skewness
kurtosis(cuckoos$breadth) %>% 
  round(0)
#kurtosis is 0 which means data is normal

descdist(cuckoos$length, discrete = FALSE, boot = 100) #data is normal
descdist(cuckoos$breadth, discrete = FALSE, boot = 100) #data is normal

group_stats <- cuckoos %>%
  group_by(species) %>%
  summarise(mean_len = mean(length, na.rm = TRUE),
            med_len = median(length, na.rm = TRUE),
            sd_len = sd(length, na.rm = TRUE),
            sum_len = sum(length),
            min_len = min(length),
            qrt1_len = quantile(length, p = 0.25),
            med_len = median(length),
            qrt3_len = median(length, p = 0.75),
            max_len = max(length),
            n_len = n())
group_stats

ggplot(cuckoos, aes(x = length, y = breadth, fill = species)) +
  geom_boxplot() +
  facet_wrap(~species)

ggplot(chicks, aes(x = weight, y = Time, fill = Diet)) +
  geom_boxplot()


plt2 <- ggplot(data = group_stats, aes(x = species, y = length)) +
  geom_bar(position = position_dodge(), stat = "identity",
           col = NA, fill = "salmon") +
  facet_wrap()
  geom_errorbar(aes(ymin = mean_wt - sd_wt, ymax = mean_wt + sd_wt),
                width = .2) +
  labs(y = "Chicken mass (g)") +
  theme_pubr()
plt2

#TEST ANOVA
cuckoo.anova <- aov(length ~ species, data = cuckoos)
summary(cuckoo.anova)
#p is smaller than 0.05 which  means there is a significant difference
#in order to determine where the difference is

TukeyHSD(cuckoo.anova)
#the biggest differences lies between Feed 3 and Feed 1

pearson <- cor.test(x = cuckoos$length, cuckoos$breadth,
         use = "everything", method = "pearson")
