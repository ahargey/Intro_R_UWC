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
library(forcats)

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

ggqqplot(cuckoos, x = "length") #checks for normality 

descdist(cuckoos$length, discrete = FALSE, boot = 100) #data is normal
descdist(cuckoos$breadth, discrete = FALSE, boot = 100) #data is normal

cuckoos_stats <- group_by(cuckoos, species) %>% 
  summarise(
    count = n(), 
    mean = mean(length, na.rm = TRUE),
    sd = sd(length, na.rm = TRUE))



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


cuckoos_box <- within(cuckoos, 
                   species <- factor(species, 
                                      levels=names(sort(table(species), 
                                                        decreasing=TRUE))))

cuckoos_box$species <- factor(cuckoos$species, levels = c("wren", "tree.pipit", "robin", "pied.wagtail", "meadow.pipit", "hedge.sparrow"))


cuckoos_test$species <- factor(cuckoos_box$species, levels = cuckoos_box$species)

cuckoos_test_1 <- with(cuckoos_test, relevel(species, "hedge.sparrow"))
ggplot(cuckoos_test, aes(x = length, y = breadth, fill = cuckoos_test_1)) + 
  geom_boxplot(aes(fill = cuckoos_test_1))

cuckoo_boxplot <- ggplot(cuckoos, aes(x = fct_reorder(species, breadth, fun = median, .desc = TRUE), y = length)) + 
  geom_boxplot(aes(fill = fct_reorder(species, breadth, fun = median, .desc = TRUE))) + 
  scale_fill_manual(values = brewer.pal(6, "Accent"), guide = guide_legend(title = "Species"), labels = c("Hedge Sparrow", 
                                                                                                          "Meadow Pipit", 
                                                                                                          "Tree Pipit", 
                                                                                                          "Pied Wagtal", 
                                                                                                          "Robin",
                                                                                                          "Wren")) +
  geom_jitter(position=position_jitter(0.2)) +
  labs(x = "Species", y = "Length (mm)", title = "Cuckoo Egg Length")
cuckoo_boxplot


 

  facet_wrap(~variable, labeller = labeller(variable = facet.names)) +
  labs(y = "Size (mm)", title = "A box plot...", subtitle = "...of the Iris data") +
  theme(axis.text.x = element_text(face = "italic"))

, labeller = labeller(variable = facet.names)) +
  labs(y = "Size (mm)", title = "A box plot...", subtitle = "...of the Iris data") +
  theme(axis.text.x = element_text(face = "italic"))

#TEST ANOVA
cuckoo.anova <- aov(length ~ species, data = cuckoos)
summary(cuckoo.anova)
#p is smaller than 0.05 which  means there is a significant difference
#in order to determine where the difference is

TukeyHSD(cuckoo.anova)
#the biggest differences lies between Feed 3 and Feed 1

pearson <- cor.test(x = cuckoos$length, cuckoos$breadth,
         use = "everything", method = "pearson")

install.packages("scatterplot3d") # Install
library("scatterplot3d") # load

colors <- c("#999999", "#E69F00", "#56B4E9","#16ccad", "#15da32", "#1c699d")
colors <- colors[as.numeric(cuckoos$species)]
scatterplot3d(cuckoos[,1:2], pch = 16, color= colors)
