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

cuckoos %>% #test for homoscedasticity
  group_by(species) %>%
  summarise(length_var = var(length),
            breadth_var = var(breadth)) 
#variance is not 2-4 times greater
#data is homoscedastic 

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

#Boxplot for egg length
cuckoo_boxplot_length <- ggplot(cuckoos, aes(x = fct_reorder(species, breadth, fun = median, .desc = TRUE), y = length)) + 
  geom_boxplot(aes(fill = fct_reorder(species, breadth, fun = median, .desc = TRUE))) + 
  scale_fill_manual(values = brewer.pal(6, "Accent"), guide = guide_legend(title = "Species"), 
                    labels = c("Hedge Sparrow", "Meadow Pipit", "Tree Pipit","Pied WagtaIl", "Robin","Wren")) +
  geom_jitter(position=position_jitter(0.2)) +
  labs(x = "Species", y = "Length (mm)", title = "Cuckoo Egg Length") +
  theme(axis.text.x = element_blank(),
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
                    labels = c("Hedge Sparrow", "Meadow Pipit", "Tree Pipit","Pied WagtaIl", "Robin","Wren")) +
  geom_jitter(position=position_jitter(0.2)) +
  labs(x = "Species", y = "Breadth (mm)", title = "Cuckoo Egg Breadth") +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.background = element_rect(fill = "#f0eae8"),
        panel.background = element_rect(fill = "#ffffff", colour = "#C0C0C0",
                                        size = 2, linetype = "solid"),
        plot.title = element_text(size=16, face="bold", hjust=0.5))
cuckoo_boxplot_breadth


#STATISTICAL ANALYSIS OF LENGTH
cuckoo_anova_length <- aov(length ~ species, data = cuckoos)
summary(cuckoo_anova_length)
#p is smaller than 0.05 which  means there is a significant difference
#in order to determine where the difference is

TK_length <- TukeyHSD(cuckoo_anova_length)

TK_length <- TukeyHSD(cuckoo_anova_length, "species", ordered = TRUE)
TKdatalength <- as.data.frame(TK_length$species, rownames = FALSE) #idk man come back
TKdatalength <- cbind(species = rownames(TKdatalength), TKdatalength)
rownames(TKdatalength) <- 1:nrow(TKdatalength) #making the index into a column

length_tk_bar <- ggplot(TKdatalength, aes(x = species, y = diff, fill = species)) +
  geom_bar(stat = "identity") +
  labs(x = "Difference in Length", y = "Species") +
  title("Tukey Analysis") +
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 1, colour = "black", size=12),
        plot.background = element_rect(fill = "#f0eae8"),
        panel.background = element_rect(fill = "#ffffff", colour = "#C0C0C0",
                                        size = 2, linetype = "solid"))

#STATISTICAL ANALYSIS OF BREADTH
cuckoo_anova_breadth <- aov(breadth ~ species, data = cuckoos)
summary(cuckoo_anova_breadth)

TK_breadth <- TukeyHSD(cuckoo_anova_breadth, "species", ordered = TRUE)
TKdatabreadth <- as.data.frame(TK_breadth$species, rownames = FALSE) #idk man come back
TKdatabreadth <- cbind(species = rownames(TKdatabreadth), TKdatabreadth)
rownames(TKdatabreadth) <- 1:nrow(TKdatabreadth) #making the index into a column

breadth_tk_bar <- ggplot(TKdatabreadth, aes(x = species, y = diff, fill = species)) +
  geom_bar(stat = "identity") +
  labs(x = "Difference in Breadth", y = "Species") +
  title("Tukey Analysis") +
  theme(legend.position = "top",
              axis.text.x = element_blank(),
              axis.text.y = element_text(hjust = 1, colour = "black", size=12),
              plot.background = element_rect(fill = "#f0eae8"),
              panel.background = element_rect(fill = "#ffffff", colour = "#C0C0C0",
                                              size = 2, linetype = "solid"))

ggarrange(length_tk_bar, breadth_tk_bar, common.legend = TRUE, legend = "top")

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

#CUCKOO HOSTS
cuckoohosts <- DAAG::cuckoohosts
cuckoohosts <- cuckoohosts[-c(7, 8, 9, 10),] #deleting species of birds for observations not recorded
cuckoohosts <- cbind(species = rownames(cuckoohosts), cuckoohosts)
rownames(cuckoohosts) <- 1:nrow(cuckoohosts) #making the index into a column

