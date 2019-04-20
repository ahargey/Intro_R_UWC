#AYESHA HARGEY
#3650393
#9th April 2019
#Basic Statistics Exercises

#Load libraries
library(tidyverse)
library(ggpubr)
library(RColorBrewer)
library(ggthemes)
library(fitdistrplus) #masks the select function, so only loaded when necessary
library(logspline)

##CHAPTER 3 EXERCISE 3.6

chicks <- datasets::ChickWeight #loading dataset

manual_summary_chicks <- chicks %>%  
  summarise(min_wt = min(weight), #minimum
            qrt1_wt = quantile(weight, p = 0.25), #quartile 1
            med_wt = median(weight), #quartile 2 / median
            qrt3_wt = quantile(weight, p = 0.75), #quartile 3
            mean_wt = mean(weight), #mean
            max_wt = max(weight)) #weight
manual_summary_chicks

##CHAPTER 4 EXERCISE 4.3
orange <- datasets::Orange

#explore data
glimpse(orange) #overall preview of data, shows every column
head(orange) #first nine rows
tail(orange) #last nine rows
nrow(orange) #number of row
ncol(orange) #number of columns
any(is.na(orange)) #is there any missing data?
summary(orange)

orange1 <- ggplot(orange, aes(x = age, y = circumference, fill = Tree)) +
  geom_boxplot(show.legend = TRUE, notch = FALSE) + 
  labs(y = "Age (days)", x = "Circumference (mm)" ) +
  coord_flip() +
  scale_fill_brewer(palette="Set2") +
  ggtitle("Boxplot") +
  theme_bw () + #modified theme 
  theme(axis.text.x = element_text(angle = 40, hjust = 1, colour = "black", size=12),
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.background = element_rect(fill = "#f0eae8"),
        plot.title = element_text(size=16, face="bold", hjust=0.5))
orange1

orange2 <- ggplot(orange, aes(x = age, y = circumference, fill = Tree)) +
  geom_violin(trim=FALSE) +
  labs(y = "Age (days)", x = "Circumference (mm)" ) +
  ggtitle("Violin Plot") + #title 
  scale_fill_brewer(palette = "Set2") + #same palette
  theme_bw () + #modified theme 
  theme(axis.text.x = element_text(angle = 40, hjust = 1, colour = "black", size=12),
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.background = element_rect(fill = "#f0eae8"),
        plot.title = element_text(size=16, face="bold", hjust=0.5))
orange2

orange3 <- ggplot(orange, aes(x = circumference, fill = Tree)) +
  geom_histogram(position = "dodge") +
  labs(y = "Count", x = "Circumference (mm)" ) +
  ggtitle("Histogram") + #title 
  scale_fill_brewer(palette = "Set2") + #same palette
  theme_bw () + #modified theme 
  theme(axis.text.x = element_text(angle = 40, hjust = 1, colour = "black", size=12),
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.background = element_rect(fill = "#f0eae8"),
        plot.title = element_text(size=16, face="bold", hjust=0.5))
orange3

orange4 <- ggplot(orange, aes(x = age, y = circumference, fill = Tree)) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_wrap(~Tree) +
  labs(y = "Circumference (mm)", x = "Age" )+
  scale_fill_brewer(palette = "Set2") +
  ggtitle("Bar Graph") +
  theme_bw () + #modified theme 
  theme(axis.text.x = element_text(angle = 40, hjust = 1, colour = "black", size=12),
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.background = element_rect(fill = "#f0eae8"),
        plot.title = element_text(size=16, face="bold", hjust=0.5))
orange4

ggarrange(orange1, orange2, orange3, orange4, common.legend = TRUE)

##CHAPTER 5 EXERCISE 5.4

#CHOOSING A DATASET FROM R
datasets::airquality

airquality <- datasets::airquality #naming the dataset

glimpse(airquality) #overall preview of data, shows every column
head(airquality) #first nine rows
tail(airquality) #last nine rows
nrow(airquality) #number of row
ncol(airquality) #number of columns
any(is.na(airquality)) #is there any missing data?
summary(airquality)

aq_temp <- airquality %>% 
  select(Temp, Day) %>% #only want these two columns
  group_by(Day)

shapiro.test(aq_temp$Temp) #tests for normality
#p value is less that 0.05 so it is not normal

descdist(aq_temp$Temp) #uniform  distribution

aq_density <- ggplot(aq_temp, aes(x = Temp)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "mediumorchid1") + #colours the columns 
  geom_density(fill = "springgreen", alpha = 0.3) + #fills in the density with a semi-transparent green
  labs(x = "Temperature (F)", y = "Density") +
  ggtitle("Uniform Distribution of Air Temperature") + #title
  theme_bw () + #modified theme 
  theme(axis.text.x = element_text(angle = 40, hjust = 1, colour = "black", size=12),
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.background = element_rect(fill = "#f0eae8"),
        plot.title = element_text(size=16, face="bold", hjust=0.5))
aq_density


#Selecting a dataset
datasets::quakes

quakes <- datasets::quakes

#Explore the data
glimpse(quakes) #overall preview of data, shows every column
head(quakes) #first nine rows
tail(quakes) #last nine rows
nrow(quakes) #number of row
ncol(quakes) #number of columns
any(is.na(quakes)) #is there any missing data?
summary(quakes)

shapiro.test(quakes$depth)

quake_density <- ggplot(quakes, aes(x = depth)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "orange1") + #colours the columns 
  geom_density(fill = "tan", alpha = 0.3) + #fills in the density with a semi-transparent green
  labs(x = "Depth  (m)", y = "Density") +
  ggtitle("Uniform Distribution of Earthquake Depths") + #title
  theme_bw () + #modified theme 
  theme(axis.text.x = element_text(angle = 40, hjust = 1, colour = "black", size=12),
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.background = element_rect(fill = "#f0eae8"),
        plot.title = element_text(size=16, face="bold", hjust=0.5))
quake_density

descdist(quakes$depth) #uniform distribution

ggarrange(aq_density, quake_density)

##CHAPTER 6 6.7
#Making a dataset
women_weight <- c(28.9, 51.2, 63.3, 21.8, 53.4, 54.6, 48.4, 38.8, 38.5)
men_weight <- c(57.8, 50, 53.4, 66, 79.4, 63.3, 57.3, 51.3, 52.4) 

weight <- data.frame(group = rep(c("Woman", "Man"), each = 9),
  weight = c(women_weight,  men_weight))

#visualizing the data
ggplot(weight, aes(x = group, y = weight)) +
  geom_boxplot() +
  labs(y = "Weight", x = "Gender") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

shapiro.test(weight$weight) 
#Data is normal

#HYPOTHESIS (TWO-SIDED)
#H0: There is a difference in the weight of men and women
#H1: There is not a difference in the weight of men and women

two_assum <- function(x) { #making the command that will allow to determine homoscedasticity
  x_var <- var(x)
  x_norm <- as.numeric(shapiro.test(x)[2])
  result <- c(x_var, x_norm)
  return(result)
}

weight_sub <- weight %>% 
  group_by(group) %>% 
summarise(weight_var = two_assum(weight)[1],
          weight_norm = two_assum(weight)[2])
weight_sub

#there is not a substantial difference so the data can be used 

compare_means(weight ~ group, data = weight, method = "t.test", var.equal = TRUE)
t.test(weight ~ group, data = weight, method = "t.test", var.equal = TRUE)

#It was found that there is a significant difference on the weight of men and women (p = 0.0158, t = 2.6998, df = 16).

#PROBABILITY DATA
bridge_crossing <- matrix(c(20, 45, 30, 52), ncol = 2)
colnames(bridge_crossing) <- c("Yes", "No")
rownames(bridge_crossing) <- c("Black", "Red")
bridge_crossing

#data created of car colour (black or red) and whether or not they crossed the bridge or not
hist(bridge_crossing) #visualizing data

#HYPOTHESIS
#H0: There will not be a difference in whether black or red cars cross the bridge 
#H1: There will be a difference in whether black or red cars cross the bridge 

prop.test(bridge_crossing) #testing proportions two-sided
#Null hypothesis is accepted 

#CONCLUSION
#It was found that there was not a significant difference on whether black or red cars crossed the bridge (p = 0.57, x squared = 0.31, df = 1).

#EXERCISE 7.4

#7.4.1
#given data
feed_1 <- c(60.8, 57.0, 65.0, 58.6, 61.7)
feed_2 <- c(68.7, 67.7, 74.0, 66.3, 69.8)
feed_3 <- c(102.6, 102.1, 100.2, 96.5)
feed_4 <- c(87.9, 84.2, 83.1, 85.7, 90.3)

#made as a dataframe
bacon <- as_tibble(data.frame(
  feed = c(
    rep("Feed 1", length(feed_1)),
    rep("Feed 2", length(feed_2)),
    rep("Feed 3", length(feed_3)),
    rep("Feed 4", length(feed_4))
  ),
  mass = c(feed_1, feed_2, feed_3, feed_4)
))

pigs_boxplot <- ggplot(bacon, aes(x = feed, y = mass)) +
  geom_boxplot(show.legend = TRUE, notch = FALSE, fill = c("tomato", "springgreen1", "olivedrab1", "pink2")) +
  labs(x = "Feed Types", y = "Mass") +
  ggtitle("Boxplot of the Effect of Feed Types on Pigs") + #title
  theme_bw () + #modified theme 
  theme(axis.text.x = element_text(angle = 40, hjust = 1, colour = "black", size=12),
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.background = element_rect(fill = "#f0eae8"),
        plot.title = element_text(size=16, face="bold", hjust=0.5))
pigs_boxplot

#HYPOTHESIS
#H0: Feed type does NOT have an effect on the mass  of pigs
#H1: Feed type does have an effect on the mass of pigs

#TWO-SIDED ANOVA

bacon.anova <- aov(mass ~ feed, data = bacon)
summary(bacon.anova)
#p is smaller than 0.05 which  means there is a significant difference
#in order to determine where the difference is

TukeyHSD(bacon.anova)
#the biggest differences lies between Feed 3 and Feed 1

#the null hypothesis is rejected
#the hypothesis is accepted

#CONCLUSION
#It was found there is a significant difference on whether feed type affects the mass of pigs (p > 0.000001, df = 3)

#7.4.2
teeth <- datasets::ToothGrowth

#Explore the data
glimpse(teeth) #overall preview of data, shows every column
head(teeth) #first nine rows
tail(teeth) #last nine rows
nrow(teeth) #number of row
ncol(teeth) #number of columns
any(is.na(teeth)) #is there any missing data?
summary(teeth)

teeth$dose = factor(ToothGrowth$dose, levels=c(0.5,1.0,2.0), labels=c("low","med","high")) #dose treated as a factor now

teeth_boxplot <- ggplot(teeth, aes(x = dose, y = len, fill = supp)) +
  geom_boxplot(show.legend = TRUE, notch = FALSE) +
  scale_fill_brewer(palette="Dark2") +
  labs(x = "Dose", y = "Length (mm)") +
  ggtitle("Boxplot of the Effect of Teeth Growth") + #title
  theme_bw () + #modified theme 
  theme(axis.text.x = element_text(angle = 40, hjust = 1, colour = "black", size=12),
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.background = element_rect(fill = "#f0eae8"),
        plot.title = element_text(size=16, face="bold", hjust=0.5))
teeth_boxplot

#HYPOTHESIS
#H0: There will be no difference in growth between the treatment of orange juice or Vitamin C(ascorbic acid)
#H1: There will be a difference in growth between the treatment of orange juice or Vitamin C

teeth_anova <- aov(len ~ supp, data = teeth) #anova analysis
summary(teeth_anova)

#P is not smaller than 0.05 so there is not a significant difference
#Null hypothesis is accepted

#CONCLUSION
#There is no significant difference in the growth of guinea pig teeth when c onsuming either orange juice or ascorbic acid (p > 0.05, df - 1)

#EXERCISE 7.4.3
#continuing the use of the ToothGrowth data

#HYPOTHESIS (two, because it's two-way)
#H0: There will be no difference in growth between the treatment of orange juice or Vitamin C(ascorbic acid) 
#H0: The amount of supplement consumed has no effect on the growth of teeth
#H1: There will be a difference in growth between the treatment of orange juice or Vitamin C
#H1: The amount of supplement consumed will have an effect on the growth of teeh

two_way_teeth <- aov(len ~ supp + as.factor(dose), data = teeth) #two way anova
summary(two_way_teeth)

#P is less than 0.05 which means there is a significance 

teeth_tukey <- TukeyHSD(two_way_teeth, which = "as.factor(dose)", conf.level = 0.90)
plot(teeth_tukey, las = 1, col = "red")

#there is a significant difference between high doses and low doses

#CONCLUSION
#There is no significant difference (P > 0.05) between the supplements given to the guinea pigs, whether it is orange juice or ascorbic acid on teeth growth.
#However there is a significant difference between the effects on length on high or low dosage (P > 0.00002, df = 2) 

#EXERCISE 9.6
#making a heatmap using ggplot
#using the default volcano dataset
#adjusting to make it into a dataframe

temp <- 

nx = 87
ny = 61
volcano_data <- data.frame(height = c(volcano), x = rep(1:nx, ny), y = rep(1:ny, each = nx))

volcano_heatmap <- ggplot(volcano_data, aes(x = x, y = y, fill = height)) + 
  geom_tile() + #makes the heatmap
  coord_fixed(expand = FALSE) +
  scale_colour_brewer(palette = "Reds") +
  ggtitle("Volcano Height") + #title
  theme_bw () + #modified theme 
  theme(axis.text.x = element_text(angle = 40, hjust = 1, colour = "black", size=12),
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.background = element_rect(fill = "#f0eae8"),
        plot.title = element_text(size=16, face="bold", hjust=0.5))
volcano_heatmap #displays the heatmap