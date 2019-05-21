# Ayesha Hargey
# 3650393
# BIOSTATS PRACTICE 

#Load libraries
library(fitdistrplus)
library(tidyverse) #loaded second to prevent masking of the select function
library(ggpubr)
library(RColorBrewer) #for palettes
library(ggthemes)
library(logspline)
library(e1071)
library(corrplot)
library(forcats)
library(reshape2)
library(Rmisc)

#Load the data
snakes <- read_csv("Basic_Stats/snakes.csv")

#turning day into a factor
snakes$day = as.factor(snakes$day)

snakes_summary <- snakes %>%  
  group_by(day) %>% 
  summarise(mean_openings = mean(openings),
            sd_openings = sd(openings))
snakes_summary

snakes.summary2 <- summarySE(data = snakes, measurevar = "openings", groupvars = c("day"))

ggplot(data = snakes, aes(x = day, y = openings)) +
  geom_segment(data = snakes.summary2, aes(x = day, xend = day, y = openings - ci, yend = openings + ci, colour = day),
               size = 2.0, linetype = "solid", show.legend = F) +
  geom_boxplot(aes(fill = day), alpha = 0.6, show.legend = F) + 
  geom_jitter(width = 0.05)

snakes.aov <- aov(openings ~ day + snake, data = snakes)
summary(snakes.aov)

par(mfrow = c(2, 2))
# Checking assumptions...
# make a histogram of the residuals;
# they must be normal
snakes.res <- residuals(snakes.aov)
hist(snakes.res, col = "red")

plot(fitted(snakes.aov), residuals(snakes.aov), col = "red")
snakes.tukey <- TukeyHSD(snakes.aov, which = "day", conf.level = 0.90)
plot(snakes.tukey, las = 1, col = "red")
