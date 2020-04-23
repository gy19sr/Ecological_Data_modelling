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
library(mnormt)
library(psych)

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

######################## Modifying Data ###############################



######## OUtliers #############


# Categorical Outliers #
#Outlier is < 10% for cat
# Worldwide shipments of smartphone OS
# in millions for 2013 Q1
OS <- read.csv("OS.csv", header = TRUE)
View(OS)
OS

# Either combine into "other" (if homogeneous) or delete
OS.hi <- subset(OS, Proportion > 0.1)
OS.hi


# Quantitative data Outliers
# See outliers in boxplots
#dplyr::filter()
require("datasets")
library(help = "datasets")
?datasets
?rivers
data.frame(rivers2)
rivers2 <- datasets::rivers # Lengths of Major North American Rivers
#data(datasets::rivers)
hist(rivers2)
boxplot(rivers2, horizontal = TRUE)
boxplot.stats(rivers2)
rivers.low  <- rivers2[rivers2 < 1210]  # Remove outliers
boxplot(rivers.low, horizontal = TRUE)  # Has new outliers
boxplot.stats(rivers.low)
rivers.low2  <- rivers2[rivers2 < 1055]  # if Remove again
boxplot(rivers.low2)  # Still one outlier


######### Transforming variables ##############


?islands
islands
hist(islands, breaks = 16) #can see a skewed distribution


# can turn into z-scores - doesn't really change, just makes easier to manage
islands.z <- scale(islands) # M = 0, SD =1
islands.z
hist(islands.z, breaks = 16) #in R breaks is a suggestion not manditory
round(mean(islands.z)) # rounds off to see M = 0
sd(islands.z) # SD = 1
attr(islands.z, "scaled:center") # Show original mean
attr(islands.z, "scaled:scale") #show original SD
islands.z <- as.numeric(islands.z) #converts from matrix back to numeric
islands.z


# Logarithmic Transformations
islands.ln <- log(islands)  # Natural log (base = e)
# islands.log10 <- log10(islands)  # Common log (base = 10)
# islands.log2 <- log2(islands)  # Binary log (base = 2)
hist(islands.ln) #more centrally distributed
boxplot(islands.ln) # far fewer outliers
#this can not be done with data with zeros
# so can Add 1 to avoid undefined logs when X = 0
# x.ln <- log(x + 1)

# Squaring
# For negatively skewed variables
# Distribution may need to be recentered so that all values are positive (0 is okay)

# Ranking
#means don't need the values just the order
islands.rank1 <- rank(islands)
hist(islands.rank1) #reason not flat is because there are ties in frequency
boxplot(islands.rank1)
# ties.method = c("average", "first", "random", "max", "min")
islands.rank2 <- rank(islands, ties.method = "random")
hist(islands.rank2)
boxplot(islands.rank2)

# Dichotomizing
# Use wisely and purposefully!
# Split at 1000 (= 1,000,000 square miles)
# ifelse is the conditional element selection
continent <- ifelse(islands > 1000, 1, 0)
continent

rm(list = ls())  # Clean up


######## Computing composite variables #######

#taking variables and combinding variables 
# Create variable rn1 with 1 million random normal values
# Will vary from one time to another
rn1 <- rnorm(1000000)
hist(rn1)
summary(rn1) #shows plus or minus 5 standard deviations

# Create variable rn2 with 1 million random normal values
rn2 <- rnorm(1000000)
hist(rn2)
summary(rn2) #shows plus or minus 5 standard deviations

# Average scores across two variables
rn.mean <- (rn1 + rn2)/2 #1st item + 1st item /2 then on to 2nd item of varaiables and so on
hist(rn.mean)

# Multiply scores across two variables
rn.prod <- rn1 * rn2
hist(rn.prod) #high frequency of middle and goes further out
summary(rn.prod) # min and max also greatly increased

# Kurtosis comparisons
# The package "moments" gives kurtosis where a
# mesokurtic, normal distribution has a value of 3.
# The package "psych" recenters the kurtosis values
# around 0, which is more common now.
#help(package = "psych")
#require("psych")
kurtosi(rn1) # should be close to zero
kurtosi(rn2) # should be close to zero
kurtosi(rn.mean) # should be close to zero
kurtosi(rn.prod)  # Similar to Cauchy distribution - looks normal but actually high skewness

# Clean up
rm(list = ls())

######## missing data ############

# NA = "Not Available"
# Makes certain calculations impossible
x1 <- c(1, 2, 3, NA, 5)
summary(x1)  # Works with NA
mean(x1)  # Doesn't work

# To find missing values
which(is.na(x1))  # Give index number

# Ignore missing values with na.rm = T
mean(x1, na.rm = T)

# Replace missing values with 0 (or other number)
# Option 1: Using "is.na"
x2 <- x1
x2[is.na(x2)] <- 0
x2
# Option 2: using "ifelse"
x3 <- ifelse(is.na(x1), 0, x1)
x3

# For data frames, R has many packages to deal
# intelligently with missing data via imputation.
# These are just three:
# mi: Missing Data Imputation and Model Checking
browseURL("http://cran.r-project.org/web/packages/mi/index.html")
# mice: Multivariate Imputation by Chained Equations
browseURL("http://cran.r-project.org/web/packages/mice/index.html")
# imputation
browseURL("http://cran.r-project.org/web/packages/imputation/index.html")

rm(list = ls())  # Clean up



############## Linear Mixed Models #####################
#Why / when
#can have different grouping factors like populations, species, sites where we collect the data
#Sample sizes small, trying to fit complicated models with many parameters
#





##############MCMCGLMM ############################





########################Bayesian############################




############## Cluster Analysis #################



















