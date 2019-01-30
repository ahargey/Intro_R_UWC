#Day 2
#Homework Assignment
#30th January 2019
#Ayesha Hargey
#3650393

#HOMEWORK:

#Load libraries
library(tidyverse)

#FIRST DATASET - speed and stopping distance of cars 

cars <- datasets::cars #assigning the dataset to the environment
??cars #information about the dataset

cars_mean_speed <- cars %>% #mean speed
  summarise(average_speed = mean(speed))

cars_mean_distance <- cars %>% #mean distance
  summarise(average_distance = mean(dist))

#HYPOTHESIS 1:
#The faster a car is travelling, the increased distance it takes to stop

#Overall graph of dataset
graph_1_cars <- ggplot(data = cars, aes(x = speed, y = dist)) + #parent line
  geom_point(aes(colour = "orange")) + #child line
  geom_smooth(method = "lm") + #line of best fit
  labs(x = "speed (km/h)", y = "distance (m)") + #labels
  ggtitle("Speed and Stopping Distances of Cars") +
  theme_bw()
graph_1_cars
            
#CONCLUSION: 
#The data suggests that the faster a car is travelling, the increased distance it takes to stop.
#This is a result of the momentum of the car. 
#Since the car was travelling faster, it takes a longer time to stop.

#SECOND DATASET: 
#measurements of the internal anatomy of 3 species of iris
iris <- datasets::iris #assigning the dataset to the environment
??iris #information about the dataset

#mean for all columns
iris_numeric <- iris %>% #species needs to be excluded from the data because it's not numeric 
  select(-Species) #because it's excluded, the colMeans function can be used

colMeans(x=iris_numeric, na.rm = TRUE) #the means of each column

#HYPOTHESIS 2: 
#The species 'Iris virginica' will have the biggest petals, compared to the other two species
graph_2_iris_petals <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point() +
  geom_smooth(method = "lm") + #line of best fit
  labs(x = "Petal Length (cm)", y = "Petal Width (cm)") +
  ggtitle("Petal Size in Species of Iris") +
  theme_bw ()
graph_2_iris_petals #displays the graph that was created

#CONCLUSION: 
#The species 'Iris virginica' has the largest petals. This could be due to the environmental factors of the flower as well as its evolutionary history.
#It might be in a place where sunlight is diminished and requires larger petals to have an increased surface area for photosynthesis
#Or the petals co-evolved with its pollinator which favoured large petals

#HYPOTHESIS 3: 
#The species 'Iris setosa' will have the smallest sepals, compared to the other two species
graph_3_iris_sepals <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_point() +
  geom_smooth(method = "lm") + #line of best fit
  labs(x = "Sepal Length (cm)", y = "Sepal Width (cm)") +
  ggtitle("Sepal Size in Species of Iris") +
  theme_bw ()
graph_3_iris_sepals #displays the graph that was created

#CONCLUSION:
#The species 'Iris setosa' has the smallest sepals compared to the other two species.
#Sepals have a role as a defensive structure. If they are small, it might suggest that the environment the flower is in is not prone to many predators.

#THIRD DATASET
library(ggpubr) #for faceting
library(boot) #for the urine dataset 

urine <- boot::urine #assigning the dataset to the environment
??urine #information about the dataset

#mean for all columns
urine_numeric <- urine %>% #'r' needs to be excluded from the data because it's a discrete value
  select(-r) #because it's excluded, the colMeans function can be used

colMeans(x=urine_numeric, na.rm = TRUE) #the means of each column

#HYPOTHESIS 4: 
#The presence of calcium oxalate crystals (r) is increased by a high concentration of urea and calcium

graph_4_urine_r <- ggplot(data = urine, aes(x = urea, y = calc)) +
  geom_point(aes(colour = as.factor(r))) +
  scale_colour_manual(values = c("black", "red"), #colour palette
                      labels = c("absent", "present")) + #legendtext
  labs(colour = "crystals") + #legend title
  geom_smooth(method = "lm") + #line of best fit
  labs(x = "Urea (millimoles per litre)", y = "Calcium concentration (millimoles per litre)") +
  ggtitle("Relationship between calcium oxalate crystals and urea and calcium concentration in urine") +
  theme_bw ()
graph_4_urine_r

#CONCLUSION: 
#There is more likely to be the presense of calcium oxalate crystals in people with a high concentration of calcium and urea
#This is most likely because it is all associated with a diet or lifestyle that contributes to this

#HYPOTHESIS 5:
#There will be a higher conductivity in the urine when there is an acidic pH and high osmolarity

graph_5_urine_cond <- ggplot(data = urine, aes(x = osmo, y = ph)) +
  geom_point(aes(colour = cond)) +
  scale_colour_gradientn(colours = c("#381B1D","#5A3443","#6F5672","#737DA0","#65A9C4","#56D6D7")) + #palette generated from ColourPicker
  labs(colour = "conductivity") + #legend title
  geom_smooth(method = "lm") + #line of best fit
  labs(x = "Osmolarity", y = "pH") +
  ggtitle("Relationship between conductivity and osmolarity and the pH of urine") +
  theme_bw () 
graph_5_urine_cond
  
#CONCLUSION:
#There is a higher conducitivty in the urine when the pH is acidic and osmolarity is higher
#This could be due to the change in ions as a result of the increased osmolarity since
#conductivity is proportional to the concentration of charged ions in the solution
#and a higher osmolarity suggets a larger solution
  
#HYPOTHESIS 5:
#A higher value of urine gravity is found with an increased osmolarity and conductivity
  
graph_6_urine_gravity <- ggplot(urine, aes(x = osmo, y = cond)) +
  geom_point(aes(colour = gravity)) +
  scale_colour_gradientn(colours = c("#383801","#5A5816","#7E7933","#A59C56","#CDBF81","#F7E4B4")) + #palette generated from ColourPicker
  labs(colour = "gravity") + #legend title
  geom_smooth(method = "lm") + #line of best fit
  labs(x = "Osmolarity", y = "Conductivity") +
  ggtitle("Relationship between the gravity and osmolarity and conductivity of urine") +
  theme_bw ()
graph_6_urine_gravity

#CONCLUSION:
#Higher gravity values are seen with an increased conductivity and osmolarity.
#The higher the gravity, the more the likelihood of dehydration
#A high conductivity could suggest this 

#LAMINARIA DATASET 
library(dplyr) #for filtering
lam <- read.csv("data/laminaria.csv") #adding laminaria to the environment

lam_mean <- lam %>% #mean length
  summarise(average_length = mean(total_length))

#GRAPH 7:
#Viewing the relationship between blade length and total length across all sites

graph_7_lam_all_sites <- ggplot(lam, aes(x = blade_length, y = total_length, colour = site)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~site, ncol=3) + #faceted so it is easier to view the differences between sites
  labs(x = "Blade Length (cm)", y = "Total Length (cm)") + #labels
  ggtitle("Relationship between blade length and total length") + #title
  theme_bw ()
graph_7_lam_all_sites

#CONCLUSION: Olifantsbos seems interesting due to the exceptionally long seaweed found there.
#I will isolate Olifantsbos and look at the data individually.

lam_olifantsbos <- lam %>%  #new dataframe
filter(site == "Olifantsbos") #data only from Olifantsbos

graph_8_lam_olifantsbos <- ggplot(lam_olifantsbos, aes(x = blade_length)) +
  geom_histogram(aes(fill = total_length), position = "dodge", binwidth = 50) +
  labs(x = "Blade Length (cm)", y = "Total Amount")+
  ggtitle("Relationship between blade length and total length in Olifantsbos") + #title
  theme_bw ()
graph_8_lam_olifantsbos #displays the plot

graph_8_lam_olifantsbos2 <- ggplot(lam_olifantsbos, aes(x = stipe_length)) +
  geom_histogram(aes(fill = total_length), position = "dodge", binwidth = 50) +
  labs(x = "Stipe Length (cm)", y = "Total Amount")+
  ggtitle("Relationship between stipe length and total length in Olifantsbos") + #title
  theme_bw ()
graph_8_lam_olifantsbos2 #displays the plot

graph_8_compiled <- ggarrange(graph_8_lam_olifantsbos, graph_8_lam_olifantsbos2)
graph_8_compiled #final graph showing the relationship of the seaweed blade and stipe length against total length