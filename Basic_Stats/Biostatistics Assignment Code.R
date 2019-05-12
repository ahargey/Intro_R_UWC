#AYESHA HARGEY
#Biostatistics Assignment 
#13 May 2019 

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

#Dataset chosen
if (!require(DAAG)) install.packages('DAAG')
library(DAAG) #datasets and functions used in the book
              #"Data Analysis and Graphics Using R"
cuckoos <- DAAG::cuckoos #naming and selecting the dataset

#Explore the data
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

ggqqplot(cuckoos, x = "length") #checks for normality visually
ggqqplot(cuckoos, x = "breadth") #checks for normality visually

descdist(cuckoos$length, discrete = FALSE, boot = 100) #data is normal
descdist(cuckoos$breadth, discrete = FALSE, boot = 100) #data is normal

cuckoos %>% #test for homoscedasticity
  group_by(species) %>%
  summarise(length_var = var(length),
            breadth_var = var(breadth)) 
#variance is not 2-4 times greater
#data is homoscedastic 

cuckoos_stats <- cuckoos %>% #breakdown of general statistics
  group_by(species) %>%
  summarise(mean_len = mean(length, na.rm = TRUE), #mean
            count = n(), #n
            sd_len = sd(length, na.rm = TRUE), #standard deviation
            sum_len = sum(length), #sum
            min_len = min(length), #minimum
            qrt1_len = quantile(length, p = 0.25), #q1
            med_len = median(length), #median
            qrt3_len = median(length, p = 0.75), #q3
            max_len = max(length)) #max
cuckoos_stats

#Boxplot for egg length
cuckoo_boxplot_length <- ggplot(cuckoos, aes(x = fct_reorder(species, breadth, fun = median, .desc = TRUE), y = length)) + 
  geom_boxplot(aes(fill = fct_reorder(species, breadth, fun = median, .desc = TRUE))) + #reordered for efficency
  scale_fill_manual(values = brewer.pal(6, "Accent"), guide = guide_legend(title = "Species"), 
                    labels = c("Hedge Sparrow", "Meadow Pipit", "Tree Pipit","Pied Wagtail", "Robin","Wren")) +
  geom_jitter(position=position_jitter(0.2)) +
  labs(x = "Species", y = "Length (mm)", title = "Cuckoo Egg Length") +
  theme(axis.text.x = element_blank(), #custom theme
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.background = element_rect(fill = "#f0eae8"),
        panel.background = element_rect(fill = "#ffffff", colour = "#C0C0C0",
                                        size = 2, linetype = "solid"),
        plot.title = element_text(size=16, face="bold", hjust=0.5))
cuckoo_boxplot_length

#Boxplot for egg breadth
cuckoo_boxplot_breadth <- ggplot(cuckoos, aes(x = fct_reorder(species, length, fun = median, .desc = TRUE), y = breadth)) + 
  geom_boxplot(aes(fill = fct_reorder(species, length, fun = median, .desc = TRUE))) + 
  scale_fill_manual(values = brewer.pal(6, "Accent"), guide = guide_legend(title = "Species"), 
                    labels = c("Hedge Sparrow", "Meadow Pipit", "Tree Pipit","Pied Wagtail", "Robin","Wren")) +
  geom_jitter(position=position_jitter(0.2)) +
  labs(x = "Species", y = "Breadth (mm)", title = "Cuckoo Egg Breadth") +
  theme(axis.text.x = element_blank(), #custom theme
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.background = element_rect(fill = "#f0eae8"),
        panel.background = element_rect(fill = "#ffffff", colour = "#C0C0C0",
                                        size = 2, linetype = "solid"),
        plot.title = element_text(size=16, face="bold", hjust=0.5))
cuckoo_boxplot_breadth

#STATISTICAL ANALYSIS OF LENGTH
#TWO-SIDED ANOVA
#HYPOTHESIS
#H0: Species of host bird does NOT have an effect on the length of cuckoo eggs
#H1: Species of host bird does have an effect on the length of cuckoo eggs

cuckoo_anova_length <- aov(length ~ species, data = cuckoos)
summary(cuckoo_anova_length)
#p is smaller than 0.05 which  means there is a significant difference
#in order to determine where the difference a Tukey test is done

TK_length <- TukeyHSD(cuckoo_anova_length)

#turning the results into a dataframe to be visually analyzed
TK_length <- TukeyHSD(cuckoo_anova_length, "species", ordered = TRUE)
TKdatalength <- as.data.frame(TK_length$species, rownames = FALSE) 
TKdatalength <- cbind(species = rownames(TKdatalength), TKdatalength)
rownames(TKdatalength) <- 1:nrow(TKdatalength) #making the index into a column

length_tk_bar <- ggplot(TKdatalength, aes(x = species, y = diff, fill = species)) +
  geom_bar(stat = "identity") +
  labs(x = "Species", y = "Difference in Length") +
  title("Tukey Analysis") +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.background = element_rect(fill = "#f0eae8"),
        panel.background = element_rect(fill = "#ffffff", colour = "#C0C0C0",
                                        size = 2, linetype = "solid"))
length_tk_bar
#STATISTICAL ANALYSIS OF BREADTH
#TWO-SIDED ANOVA
#HYPOTHESIS
#H0: Species of host bird does NOT have an effect on the breadth of cuckoo eggs
#H1: Species of host bird does have an effect on the breadth of cuckoo eggs
cuckoo_anova_breadth <- aov(breadth ~ species, data = cuckoos)
summary(cuckoo_anova_breadth)
#p is smaller than 0.05 which  means there is a significant difference
#in order to determine where the difference a Tukey test is done

TK_breadth <- TukeyHSD(cuckoo_anova_breadth, "species", ordered = TRUE)
TKdatabreadth <- as.data.frame(TK_breadth$species, rownames = FALSE)
TKdatabreadth <- cbind(species = rownames(TKdatabreadth), TKdatabreadth)
rownames(TKdatabreadth) <- 1:nrow(TKdatabreadth) #making the index into a column

breadth_tk_bar <- ggplot(TKdatabreadth, aes(x = species, y = diff, fill = species)) +
  geom_bar(stat = "identity") +
  labs(x = "Species", y = "Difference in Breadth") +
  title("Tukey Analysis") +
  theme(legend.position = "top",
              axis.text.x = element_blank(),
              axis.text.y = element_text(hjust = 1, colour = "black", size=12),
              plot.background = element_rect(fill = "#f0eae8"),
              panel.background = element_rect(fill = "#ffffff", colour = "#C0C0C0",
                                              size = 2, linetype = "solid"))
breadth_tk_bar

ggarrange(length_tk_bar, breadth_tk_bar, common.legend = TRUE, legend = "top") #combined graph

#CORRELATION
pearson_cuckoos <- cor.test(x = cuckoos$length, cuckoos$breadth)
pearson_cuckoos #0.5 slightly strong

r_print <- paste0("r = 0.5")
correlation_cuckoos <- ggplot(data = cuckoos, aes(x = length, y = breadth)) +
  geom_smooth(method = "lm", colour = "slategray2", se = F) +
  geom_point(colour = "tomato2") +
  geom_label(x = 20, y = 17.3, label = r_print) +
  labs(x = "Egg length (mm)", y = "Egg breadth (mm)") +
  theme_pubclean() 
correlation_cuckoos 