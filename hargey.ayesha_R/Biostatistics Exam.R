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

health_anova <- aov(ms_log ~ Substance, data = health_transformed)
summary(health_anova)
#p is <0.05 which means there is a significant difference for both variables
#A Tukey analysis is done to determine where this difference lies
TukeyHSD(health_anova)

plot(health_anova, 1)
#points 3, 330 and 166 are outliers

#Higher scores represent better health therefore:
#Heroin has the worst effect on mental score

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
  annotate("text", x = 0.1, y = 125, label = paste0(chisquare_health$p.value), parse = TRUE, hjust = 0) + #p value pasted in
  annotate("text", x = 0.05, y = 125, label = paste0("p"), parse = TRUE, hjust = 0) +
  theme_pubclean()
health_boxplot_suicide

health_suicide <- health_transformed %>% 
  select(Substance, Suicide_risk) #subsetting data just to substance and suicide
#a Chi square is done

as.matrix(health_suicide) #converting to a matrix
chisquare_health <- chisq.test(table(health_suicide)) #chisquare

round(chisquare_health$residuals, 3) #Pearson residuals 
health_corrplot <- corrplot(chisquare_health$residuals, is.cor = FALSE) #Pearson residuals visuzalized
corrplot(chisquare_health$residuals, is.cor = FALSE)

#The size of the residual determines how much they contributed to the overall Chi square
#And the greatest effect they have 

#The drug that has the most severe effect on suicide risk is alcohol

#E)
health_suicide_sex <- ggplot(data = health_transformed, aes(x = Suicide_risk)) +
  geom_bar(aes(fill = Substance), position = "dodge", stat = "count") +
  facet_wrap(~Sex) +
  scale_fill_brewer(palette = "Set2", guide = guide_legend(title = "Substance")) +
  labs(x = "Risk of Suicide", y = "Count", title = "The relationship of suicide risk and substance abused in terms of sex") +
  theme_pubclean()
health_suicide_sex

health_corrplot
compare_means(ms_log ~ Substance, data = health_transformed, method = "anova", var.equal = TRUE, alternative = "greater")
#The safest drug for men would be cocaine. It has a strong negative correlation with suicide.
#It also has the lowest risk on mental health.
#THe safest drug for women would be heroin. 

#F)
#H0: There is no difference in mental health between males and females depending on the drug abused 
#H1: There is a difference in mental health between males and females depending on the drug abused

health_mentalhealth <- ggplot(data = health_transformed, aes(x = Sex, y = ms_log)) +
  geom_boxplot(aes(fill = Substance)) +
  scale_fill_brewer(palette = "Set2") +
  labs(x = "Sex", y = "Log of MCS", title = "The relationship of MCS and substance abused according to gender") +
  theme_pubclean()
health_mentalhealth

#A two tailed ANOVA is done
mentalhealth_anova <- aov(ms_log ~ Substance + Sex, data = health_transformed)
summary(mentalhealth_anova)
#p is <0.05 which means there is a significant difference for both variables
#A Tukey analysis is done to determine where this difference lies
TukeyHSD(mentalhealth_anova)
#there is a signifcant difference between male and female mental health

plot(mentalhealth_anova, 1)
#CONCLUSION: There is a significant difference between male and female mental health depending on the substance abused (P < 0.0041, df = 1).
#Males are more at risk from suffering from impaired mental health than females

#G
#Correlation 
#H0: There is no relationship between age and CESD 
#H1: There is a casual relationship between age and CESD


#A Kendall Rank correlation is used as it works for both continous and ordinal data
cor.test(health_transformed$Age, health_transformed$CESD, method = "kendall")

r_print <- paste0("p = 0.7643")

ggplot(data = health_transformed, aes(x = Age, y = CESD)) +
  geom_smooth(method = "lm", colour = "lightsteelblue4", se = F) +
  geom_point(colour = "lemonchiffon4") +
  geom_label(x = 20, y = 60, label = r_print) +
  labs(x = "Age (years)", y = "CESD (scores)", title = "Kendall Rank Correlation of age and CESD scores") +
  theme_pubclean()
                  
#p value is 0.7643
#p > 0.05

#Conclusion:
#There is no significant relationship between age and CESD (p = 0.7643, z = 0.299) 

#H)
#H0: There is no relationship between CESD and mental scores
#H1: There is a relationship between CESD and mental scores
#Regression
#Linear regression used because it's testing the significance of the dependence of one continuous variable on another

health_transformed2 <-  health %>% #CESD is also being log transformed
  mutate(ms_log = log(Mental_score),
         log_CESD = log(CESD))

health_lm <- lm(ms_log ~ log_CESD, data = health_transformed2)
summary(health_lm)

slope_health <- round(health_lm$coef[2], 3)
p.value = "<0.00001"
r2_health <- round(summary(health_lm)$r.squared, 3)

ggplot(data = health_transformed2, aes(x = ms_log, y = log_CESD)) +
  geom_point(colour = "grey60") +
  annotate("text", x = 2, y = 3, label = paste0("slope == ", slope_health), parse = TRUE, hjust = 0) +
  annotate("text", x = 2, y = 4.75, label = paste0("italic(p)", p.value), parse = TRUE, hjust = 0) +
  annotate("text", x = 2, y = 4.5, label = paste0("italic(r)^2 == ", r2_health), parse = TRUE, hjust = 0) +
  stat_smooth(method = "lm", colour = "tan2") +
  labs(title = "Linear regression of CESD scores and mental health", x = "Log of Mental Scores",
       y = "Log of CESD scores")

#Conclusion: The null hypothesis is rejected and the hypothesis is accepted. 
#There is a significant difference in the relationship between CESD and mental scores, suggesting that the two are related to each other (P <0.00001, r = 0.3735)

#I) 
#H0: There is no difference between mental score and social support
#H1: There is a difference between mental score and social support 

health_transformed3 <-  health %>% #social support is also being log transformed
  mutate(ms_log = log(Mental_score),
         log_CESD = log(CESD),
         log_ss = log(Social_support))

cor.test(health_transformed3$ms_log, health_transformed3$log_ss, method = "kendall")

r_print2 <- paste0("p = 0.006")

health_corr <- ggplot(data = health_transformed3, aes(x = ms_log, y = log_ss)) +
  geom_smooth(method = "lm", colour = "indianred4", se = F) +
  geom_point(colour = "ivory4") +
  geom_label(x = 2.1, y = 2.4, label = r_print2) +
  labs(x = "Log of MCS", y = "Log of Social Support (scores)", title = "Kendall Rank Correlation of Social Support and Mental Health") +
  theme_pubclean()
health_corr

#Conclusion: There is a significant difference between mental score and social support (p > 0.006, z = 2.71).
#The null hypothesis is rejected.

#J: ABSTRACT

# This study investigated adult inpatients from a detoxification unit between the ages of 18 and 60, and abused one of three substances, either alcohol, heroin or cocaine. 
# The inpatients were interviewed continuously, and data was recorded from them, including their age, primary substance of abuse and sex. 
# Further data recorded includes their suicide risk, their CESD scores (which quantify depression), their SF-36 Mental Component Score (which represents better health) and their social support. 
# A statistical analysis was conducted in R, where ANOVA, a linear regression and a Kendall Rank correlation was done. 
# The mean age of individuals was 33 years. 
# Males are more likely than females to abuse substances, and there are more older and younger males than females.  
# There is a significant difference on the mental scores of patients and the drug they abuse, with those that use cocaine to have the highest scores suggesting better health. 
# The highest suicide risk is found in those individuals that use alcohol, and the lowest is in those that use cocaine. 
# This is true for both men and women, however women have a slightly more reduced suicide risk when partaking heroin. 
# Heroin does not have a high effect on suicide risk compared to alcohol. 
# There is no significant correlation between age and the CESD scores. 
# However, there is a significant relationship between CESD scores and Mental Component Scores. 
# There is also a significant correlation between social support and mental component scores, suggesting those that have a strong support system have better mental health.  

# Keywords: substance abuse, mental health, detoxification, cocaine, alcohol, heroin

# QUESTION 2