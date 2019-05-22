library(fitdistrplus)
library(tidyverse) #loaded second to prevent masking of the select function
library(ggpubr)
library(RColorBrewer) #for palettes
library(ggthemes)
library(logspline)
library(e1071)

chicks <- datasets::ChickWeight

#log transformed means no negative values and no zeroes
chicks_transformed <-  chicks %>% #mutate creates new column
  mutate(log10 = log10(weight), #log10 has no negatives or zeroes
         sqr = sqrt(weight), #squareroot works with zeroes or minuses
         log = log(weight)) #play it safe and use log for bigger datasets

#make sure code makes reference to the new column 

#sometimes only a single column is not normal
#then you can just transform a single column
#or the entire dataset 
#do as many things
#have read.csv file explicitly stated
#try out the snakes dataset

#might ask to recreate same plot
#write hypothesis and null hypothesis

#regressions are 'not' favoured
#what is a regression:
#when the dependence of one variable is compared as a result of another variable
#what and why are you comparing it

#what is a correlation:
#correlations compare two different variables in the same sample i.e like height and age

#three types of correlations, same code 
#Pearson - when the values are continuous 
#Spearman - not continuous, ordinal data
#Kendall-Rank - test will work for ordinal and continuous data 
#kendall is safe to use

#messy data
#no gaps, no open cells, long data not wide data
#put empty cells as 'na' 
#points not commas

#do all the normality tests #shapiro
#just log transform everything

#T TESTS

#one-sampled t-test: one value against the population mean #only one sample collected
#two-sample t-test: two values against the population mean #two samples is always used

#"two sample t test because multiple samples were recorded"

#go through the snakes dataset 
