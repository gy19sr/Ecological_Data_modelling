#Script

rm(list = ls()) #clear work space
library(tidyverse)
library(sf)
library(tmap)
library(GISTools)
library(rgdal)
library(sp)
library(RColorBrewer)
library(reshape2)
library(rpart)
library(rpart.plot)
library(visNetwork)
library(caret)
library(randomForest)
library(ranger)
library(sparkline)


# Set your working directory
setwd("C:/Users/stuar/OneDrive/Documents/onlinecourses/Ecology/Ecological_Data_modelling")  
# If you're working in an R project, skip this step

# LOAD DATA
trees <- read.csv(file = "trees.csv", header = TRUE)

head(trees) # make sure the data imported OK, familiarise yourself with the variables

#dplyr::filter() -- as there are conflicts
#setwd("")

#trees = as_tibble(read.csv(file = "trees.csv", header = TRUE, stringsAsFactors = F))
#head(trees)  
#str(trees)


# Count the number of trees for each species


# create an internal grouping structure, so that the next function acts on groups 
# (here, species) separately.
trees.grouped <- group_by(trees, CommonName)   

# here we use length to count the number of rows (trees) for each group (species). 
#trees.summary <- summarise(trees.grouped, count = length(CommonName))   

#--- Alternatively----, dplyr has a tally function  does the counts 
trees.summary <- tally(trees.grouped)


##--- now dplyr method ---
# Count the number of trees for each species, with a pipe!
trees.summary <- trees %>%                   # the data frame object that will be passed in the pipe
  group_by(CommonName) %>%    # don't need to name the object, just the grouping variable
  tally()                     

# ctrl shift M for %>% 

trees.subset <- trees %>%
  filter(CommonName %in% c('Common Ash', 'Rowan', 'Scots Pine')) %>% #three species
  group_by(CommonName, AgeGroup) %>% #age group and 
  tally() #count of each type
print(trees.subset)


#summary dataframe
summ.all <- summarise_all(trees, mean)

unique(trees$LatinName)  # Shows all the species names

trees.genus <- trees %>%
  mutate(Genus = case_when(               # creates the genus column and specifies conditions
    grepl("Acer", LatinName) ~ "Acer",
    grepl("Fraxinus", LatinName) ~ "Fraxinus",
    grepl("Sorbus", LatinName) ~ "Sorbus",
    grepl("Betula", LatinName) ~ "Betula",
    grepl("Populus", LatinName) ~ "Populus",
    grepl("Laburnum", LatinName) ~ "Laburnum",
    grepl("Aesculus", LatinName) ~ "Aesculus", 
    grepl("Fagus", LatinName) ~ "Fagus",
    grepl("Prunus", LatinName) ~ "Prunus",
    grepl("Pinus", LatinName) ~ "Pinus",
    grepl("Sambucus", LatinName) ~ "Sambucus",
    grepl("Crataegus", LatinName) ~ "Crataegus",
    grepl("Ilex", LatinName) ~ "Ilex",
    grepl("Quercus", LatinName) ~ "Quercus",
    grepl("Larix", LatinName) ~ "Larix",
    grepl("Salix", LatinName) ~ "Salix",
    grepl("Alnus", LatinName) ~ "Alnus")
  )


trees.genus.2 <- trees %>% 
  tidyr::separate(LatinName, c("Genus", "Species"), sep = " ", remove = FALSE) %>%  
  dplyr::select(-Species)


trees.genus <- trees.genus %>%   # overwriting our data frame 
  mutate(Height.cat =   # creating our new column
           case_when(Height %in% c("Up to 5 meters", "5 to 10 meters") ~ "Short",
                     Height %in% c("10 to 15 meters", "15 to 20 meters") ~ "Medium",
                     Height == "20 to 25 meters" ~ "Tall")
  )

head(trees.genus)

## Reordering a factor's levels

levels(trees.genus$Height.cat)  # shows the different factor levels in their default order

trees.genus$Height.cat <- factor(trees.genus$Height.cat,
                                 levels = c('Short', 'Medium', 'Tall'),   # whichever order you choose will be reflected in plots etc
                                 labels = c('SHORT', 'MEDIUM', 'TALL')    # Make sure you match the new names to the original levels!
)   

levels(trees.genus$Height.cat)  # a new order and new names for the levels


# Subset data frame to fewer genera

trees.five <- trees.genus %>%
  filter(Genus %in% c("Acer", "Fraxinus", "Salix", "Aesculus", "Pinus"))

# Map all the trees

(map.all <- ggplot(trees.five) +
    geom_point(aes(x = Easting, y = Northing, size = Height.cat, colour = Genus), alpha = 0.5) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 12),
          legend.text = element_text(size = 12))
)

# Plotting a map for each genus

tree.plots <-  
  trees.five  %>%      # the data frame
  group_by(Genus) %>%  # grouping by genus
  do(plots =           # the plotting call within the do function
       ggplot(data = .) +
       geom_point(aes(x = Easting, y = Northing, size = Height.cat), alpha = 0.5) +
       labs(title = paste("Map of", .$Genus, "at Craigmillar Castle", sep = " ")) +
       theme_bw() +
       theme(panel.grid = element_blank(),
             axis.text = element_text(size = 14),
             legend.text = element_text(size = 12),
             plot.title = element_text(hjust = 0.5),
             legend.position = "bottom")
  ) 

# You can view the graphs before saving them
tree.plots$plots

# Saving the plots to file

tree.plots %>%              # the saving call within the do function
  do(., 
     ggsave(.$plots, filename = paste(getwd(), "/", "map-", .$Genus, ".png", sep = ""), device = "png", height = 12, width = 16, units = "cm"))


############## Linear Mixed Models #####################
#Why / when
#can have different grouping factors like populations, species, sites where we collect the data
#Sample sizes small, trying to fit complicated models with many parameters
#





##############MCMCGLMM ############################





########################Bayesian############################




############## Cluster Analysis #################



















