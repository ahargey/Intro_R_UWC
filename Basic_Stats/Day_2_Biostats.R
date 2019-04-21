#AYESHA HARGEY
#3650393
#15th April 2019
#Basic Statistics - Day 2 

#Finding data distribution
#Load libraries
library(tidyverse)
library(fitdistrplus)
library(logspline)
library(ggplot2)

y <- c(18,9,31,7,47,28,20,300,19,6,19,21,99,85,52,68,69,3,48,116,15,27,51,100,105,99,73,58,1,89,222,56,27,36,300,121,5,42,184,88,24,127,67,93,85,60,92,23,39,140,60,71,333,42,16,51,151,625,624,200,350,4,105,199,88,742)
#data used

x <- c(37.50,46.79,48.30,46.04,43.40,39.25,38.49,49.51,40.38,36.98,40.00,
       38.49,37.74,47.92,44.53,44.91,44.91,40.00,41.51,47.92,36.98,43.40,
       42.26,41.89,38.87,43.02,39.25,40.38,42.64,36.98,44.15,44.91,43.40,
       49.81,38.87,40.00,52.45,53.13,47.92,52.45,44.91,29.54,27.13,35.60,
       45.34,43.37,54.15,42.77,42.88,44.26,27.14,39.31,24.80,16.62,30.30,
       36.39,28.60,28.53,35.84,31.10,34.55,52.65,48.81,43.42,52.49,38.00,
       38.65,34.54,37.70,38.11,43.05,29.95,32.48,24.63,35.33,41.34)
length(x)

z <- rnorm(100,13,2)
length(z)
mean(z)
hist(z)


par(mfrow = c(2, 2)) #parameters
plot(x = c(1:length(y)), y = y)
hist(y)
descdist(y, discrete = FALSE, boot = 100)
windows() #makes a new window to plot 
dev.off()

descdist(y, discrete = FALSE, boot = 100)
windows()

y <- rnorm(10, 13, 2)
descdist(y, discrete = FALSE, boot = 100) #cullen and frey
windows()
hist(y)

v <-rnorm(n = 200, m = 13, sd = 2)
par(mfrow = c(2, 2))
#base code R histogram
hist(v, main = "Histogram of observed data")
plot(density(v), main = "Density estimate of data")
plot(ecdf(v), main = "Empirical cumulative distribution function")
#standardised data
z.norm <- (v - mean(v)) / sd(v) 
#makes a qqplot
qqnorm(z.norm)
#adds a 45-degree reference line
abline(0, 1)

# Random normal data
set.seed(666)
r_dat <- data.frame(dat = c(rnorm(n = 1000, mean = 10, sd = 3),
                            rnorm(n = 1000, mean = 8, sd = 2)),
                    sample = c(rep("A", 1000), rep("B", 1000)))
tail(r_dat)

h <- ggplot(data = r_dat, aes(x = dat, fill = sample)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = sample), colour = NA, alpha = 0.4) +
  labs(x = "value")
h

shapiro.test(r_dat$dat) #if p value is less than 0.5, reject the null hypothesis
                        #if p value is more than 0.5, accept the null hypothesis
#^this code is wrong as it's pulling all data from dat
#not separated by sample

#tidyverse way
r_dat %>% 
  group_by(sample) %>% 
  summarise(norm_dat = as.numeric(shapiro.test(dat)[2])) 
#2 selects only the p value i.e the second column value

#answers are less than 0.5 so it means that the null hypothesis reject

str(r_dat$sample)
 
#structure function can look at the internal arrangements of an R object

#create random normal data
set.seed(666)
r_one <- data.frame(dat = rnorm(n = 20, mean = 20, sd = 5),
                    sample = "A")

#check normality
shapiro.test(r_one$dat) #data is norrmal because p > 0.05

t.test(r_one$dat, mu = 15) 

#data visualization
ggplot(data = r_one, aes(y = dat, x = sample)) +
  geom_boxplot(fill = "lightsalmon") +
  #population  mean (mu) = 20 #mean being close to median suggests normal distribution
  geom_hline(yintercept = 20, colour = "blue", 
             size = 1, linetype = "dashed") +
  #population  mean (mu) = 30
  geom_hline(yintercept = 30, colour = "red", 
             size = 1, linetype = "dashed") +
  labs(y = "Value", x = NULL) +
  coord_flip()

#TWO SIDED TEST
t.test(r_one$dat, mu = 30, alternative = "greater")
#argument
#are the data less than 30
#the null hypothesis has to be rejected

#TWO SAMPLE
set.seed(666)
r_two <- data.frame(dat = c(rnorm(n = 20, mean = 4, sd = 1),
                            rnorm(n = 20, mean = 5, sd = 1)),
                    sample = c(rep("A", 20), rep("B", 20)))

t.test(dat ~ sample, data = r_two, var.equal = TRUE)
#tilde means "as a function of"
#dat ~ sample is what identifies it as a two type

#ANOVA
ChickWeight
chicks <- as_tibble(ChickWeight) #load data
#this is dependent data because it's same chick measured

chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)

#NULL HYPOTHESIS: DIET 1 HAS  NO DIFFERENCE TO DIET 2
#a t test is used because there's two samples 
#independent because it's different chickens measured 
#two sample independent two sided t-test 

#THE CHICKENS FED DIET ONE WILL HAVE A LOWER MASS THAN THE CHICKENS FED DIET TWO

t.test(weight ~ Diet, data = chicks_sub)

#Do any of the four diets have an impact on the weight of the chicken
#Filter cuts down the number of rows
chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21)) #has all 4 diets
summary(chicks.aov1) #output of anova 

#Pr is less than 0.05
#null hypothesis rejected
#alternative hypothesis accepted
#there is a difference between the groups of chickens that were fed one of 4 different diets
#weighed more or less than other diets 
#the diet influenced the final mass of the chicken at 21 days 

#NULL HYPOTHESIS NEEDS TO BE UNAMBIGUOUS 
#There is not going to be a difference in the weight of the chickens according to the diets

#make a graph where it can be seen which diet has noticable effect + with notches

chicks_bp <- filter(chicks, Time == 21)

ggplot(chicks_bp, aes(x = Diet, y = weight)) +
  geom_boxplot(aes(fill = Diet), notch = TRUE, notchwidth = 0.5) + 
  labs(x = "Diet", y = "Final Mass (g)") +
  ggtitle("Chick Weights")

#where the notches overlap are areas of similarities 

TukeyHSD(chicks.aov1, ordered = TRUE)
#compares each diet to each diet 
#gives the difference of each lower and upper confidence interval is
#in this case 3 and 1 is the biggest difference
#which makes sense with what is seen in the graph
#work through tasks

summary(aov(weight ~ Diet, data = filter(chicks, Time %in% c(21))))

TukeyHSD(aov(weight ~ Diet, data = filter(chicks, Time %in% c(2))))

summary(aov(weight ~ Diet * as.factor(Time), data = filter(chicks, Time %in% c(0, 21))))
TukeyHSD(aov(weight ~ Diet + as.factor(Time), data = filter(chicks, Time %in% c(0, 21))))

# Neat
# A bit more comments will make it easier for someone else to read
# Its also good to write a mini info describing the script at the top of the script
# Nicely done
