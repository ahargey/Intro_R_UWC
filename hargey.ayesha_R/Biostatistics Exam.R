#AYESHA HARGEY
#3650393
#Biostatistics Exam
#BCB700
#22 May 2019

#Load libraries
library(fitdistrplus)
library(tidyverse) #loaded second to prevent masking of the select function
library(ggpubr)
library(RColorBrewer) #for palettes
library(ggthemes)
library(logspline)
library(e1071)
library(corrplot)
library(ggthemes)

#QUESTION ONE: HEALTH DATA
#Load the dataset 
health <- read_delim("hargey.ayesha_R/health.csv", 
                     ";", escape_double = FALSE, trim_ws = TRUE)
View(health)

#Explore the data 
glimpse(health) #overall preview of data, shows every column
head(health) #first six rows
tail(health) #last six rows
nrow(health) #number of row
ncol(health) #number of columns
dim(health) #dimensions of data
any(is.na(health)) #is there any missing data?
summary(health) #gives a summary of the mean, median, quartiles and min/max values

#A)
#Independent variables
#Substance abused

#Dependent variables:
#Age, CESD, Sex, Social Support, Mental Score, Suicide Risk

#B)
health %>% nrow()
#There are 453 individuals in this sample

health %>% count("Sex")
#There are 107 females and 346 males

#C)
range(health$Age)
count(health$Age)

health_f <- health %>% filter(Sex == "female") #females only dataset
count(health_f$Age)

#maybe can do a density plot here will see later
health_age <- ggplot(data = health, aes(x = Age)) +
  geom_histogram(aes(fill = Sex), position = "dodge", binwidth = 100, stat = "count") +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Age (years)", y = "Count", title = "Age Range and Sex") +
  theme_pubclean() #publication ready theme
health_age

health_age_facet <- ggplot(data = health, aes(x = Age)) +
  geom_histogram(aes(fill = Sex), position = "dodge", binwidth = 100, stat = "count") +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Age (years)", y = "Count", title = "Age Range and Sex") +
  facet_wrap(~Sex) +
  theme_pubclean() #publication ready theme
health_age_facet

health_combined <- ggarrange(health_age, health_age_facet)
health_combined

#the age range is from age 19 to age 60
#the most widely represented age gap is between 30 and 40 years old
#age 33 has the most individuals for both males and females 
#there are not very many older females in this study, with the oldest being 58
#men are both the youngest and the oldest

#NORMALITY TESTS
descdist(health$Mental_score, discrete = FALSE, boot = 100)
shapiro.test(health$Mental_score) #normality test
#p is < 0.05, so the data is not normally distributed
#therefore data transformation is needed

health_transformed <-  health %>% #column will be log transformed
  mutate(ms_log = log(Mental_score))

shapiro.test(health_transformed$ms_log)
#p is now > 0.05, and the data is normal

#HOMOSCEDASTICITY
health_transformed %>% #test for homoscedasticity
  group_by(Substance) %>%
  summarise(length_ms_log = var(ms_log))
#variance is not 2-4 times greater
#data is homoscedastic 

#HYPOTHESIS 
#H0: There is no difference between the drug effects on suicide risk of patients 
#H1: There is a difference between the drug effects on suicide risk of patients 

#a two-tailed anova is done 
#I predict that there is an interaction between these two variables
#And that those with lower mental scores, are more likely to be at risk of suicide
health_interaction_anova <- aov(ms_log ~ Suicide_risk * Substance, data = health_transformed)
summary(health_interaction_anova)

#It was found the interaction was not significant, so the additive ANOVA was used

health_anova <- aov(ms_log ~ Substance + Suicide_risk, data = health_transformed)
summary(health_anova)
#p is <0.05 which means there is a significant difference for both variables
#A Tukey analysis is done to determine where this difference lies
TukeyHSD(health_anova)

plot(health_anova, 1)
#points 3, 330 and 166 are outliers

#Heroin has the biggest risk on mental score
#Alcohol has the biggest effect on suicide risk


health_boxplot_mcs <- ggplot(data = health_transformed, aes(x = Substance, y = ms_log)) +
  geom_boxplot(aes(fill = Substance), show.legend = F) +
  geom_jitter(width = 0.05) +
  stat_compare_means(method = "anova", label.y = 4.3) + #p value
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Substance", y = "Log of MCS", title = "The relationship of MCS and substance abused") +
  theme_pubclean()
health_boxplot_mcs

#H0: There is no difference between the drug effects on a patient's mental component scores 
#H1: There is a difference between the drug effects on patient's mental component scores 

health_boxplot_suicide <- ggplot(data = health_transformed, aes(x = Substance)) +
  geom_bar(aes(fill = Suicide_risk), position = "dodge", stat = "count") +
  scale_fill_brewer(palette = "Set2", guide = guide_legend(title = "Risk of Suicide")) +
  labs(x = "Substance", y = "Suicide Risk", title = "The relationship of suicide risk and substance abused") +
  theme_pubclean()
health_boxplot_suicide

health_suicide <- health_transformed %>% 
  select(Substance, Suicide_risk) #subsetting data just to substance and suicide
#a Chi square is done

as.matrix(health_suicide) #converting to a matrix
chisq.test(table(health_suicide))



