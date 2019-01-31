#DAY 3
#Homework
#AYESHA HARGEY
#3650393
#31 January 2019 

#Homework: convert CSV to .Rdata. Write one line of code to do this.
#First load your CSV into R and then convert.
#Write a 5 line paragraph discusing the library ggsn and library scales. Description.  
#Practice how to read code

# Load libraries
library(tidyverse)

#1
lam <- read.csv("data/laminaria.csv")
save(lam,file = "data/laminaria.RData")

#2 
library(ggsn)
library(scales)

#The above two libraries are packages found in R. 
#They are used for map-making and plotting. 
#The ggsn package has the ability to add different types of north symbols and scale bars, all which can enhance maps.
#The scales package enhances the aesthetics of maps.
#It provides a way to automatically determine breaks and labels for axes and legends.
#Both these libraries are used to make maps.


