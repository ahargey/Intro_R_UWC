# Instructions:
# Submit: Tuesday morning (before 10-am)
# Answer each of the sections in an individual script (titled section 1,2,3)
# Answer all sections
# Add comments and notes thoughout the script
# Have details at the top of each script


# Section 1: always make maps for both rast_feb and rest_aug
# Make use of the rast_feb and rast_aug dataset:
# Explore the dataset (Hint* head, tail, !dims, glimpse, !summary etc) - Make use of google for more functions on exploring a dataset
# Create a map by making use of the lat and long variables
# Create a colour pallete using the link in the document and make use this colour pallete on the map
# Add complete labels and titles to the map
# Add the name of the oceans (Atlanic and indian ocean) on the map, increase the size of the labels
# The map should include the north arrow and scale bar
# Bonus marks for insetting (having a smaller map inside another map)
# Get creative, try new things.

# Section 2: 
# Make use of the ecklonia.csv dataset:
# Explore the data (Hint* head, tail, glimpse functions)
# Demonstrate the dimensions of the dataset (dim)
# Create three graphs; bargraph, line graph and boxplot: Write hypothesis for each of the graphs and answer these hypotheses
# Make use of the ggarrange function and arrange these three graphs created above into 1 plot
# All graphs must have labels as well as titles !and themes!
# Calculate the mean,max,min,median and variance for the stipe_length, stipe_diameter for each of the sites (Hint* group_by site)
# Calculate standard error !se!
# #lam %>% #standard error
# group_by(site) %>%
#   summarise(var_bl = var(blade_length),
#             n = n()) %>%
#   mutate(se = sqrt(var_bl/n)) #creates a new column

# Determine the min and maximum frond length and stipe length
# Determine the overall summary of the dataset !summary(wholedatasetname)

# Section 3: 
# Make use of the SACTN_day1 data:
# Here create a graph showing temperature variation between sites !(group by site!)
# Select all the temperatures recorded at the site Port Nolloth during August or September.
# Select all the monthly temperatures recorded in Port Nolloth during the year 1994
# Calculate the average temperature by depth
# Work through the tidyverse section within the document. Show what you have done by creating comments/ notes throughout the script
# FROM TIDY TO TIDIEST

# Section 4:
# Make use of any two built in datasets:
# Make use of the summarise, select, group_by functions ('in one code or different chunks')
# Create at least two visualisations that were not done in the Intro R workshop eg. density plot, 
# !go to geom_ and look from there

## Good luck!!!
geom_
---------------------------------------------------------------------------------------------