# Introduction----

# Tutorial 4 code #

# Nicole Yap s1761850@ed.ac.uk

# 16-10-2020

# LIBRARIES and loading data----
library(dplyr)     # for data manipulation
library(ggplot2)   # for making graphs
library(tidyr)

# Load data
trees <- read.csv(file = "Data/trees.csv", header = TRUE)
head(trees) #checking if data was imported correctly 

# Count the number of trees for each species by creating extra data frame trees.grouped----

# create an internal grouping structure, so that the next function acts on groups (here, species) separately. 
trees.grouped <- group_by(trees, CommonName)

# we use length to count the number of rows (trees) for each group (species). We could have used any column name e.g AgeGroup
trees.summary <- summarise(trees.grouped, count = length(CommonName)) 
#trees.summary2 <- summarise(trees.grouped, count = length(AgeGroup))

# Alternatively, dplyr has a tally function that does the counts for you!
trees.summary <- tally(trees.grouped)

# Count the number of trees for each species using a pipe!----

trees.summary <- trees %>%                   # the data frame object that will be passed in the pipe
  group_by(CommonName) %>%    # we don't need to name the object, just the grouping variable
  tally()                     # we don't need anything at all here, it has been passed through the pipe

# Using dplyr functions in pipes----

trees.subset <- trees %>%
  filter(CommonName %in% c('Common Ash', 'Rowan', 'Scots Pine')) %>% # subsetting the data frame to the three species
  group_by(CommonName, AgeGroup) %>%   # counting the number of trees for each species and breaking them down by age group.
  tally()

# Generating a summary dataframe
summ.all <- summarise_all(trees, mean)

# Using case_when() reclassifying values or factors 
# case_when() is a generalisation of ifelse() that lets you assign more than two outcomes

vector <- c(4, 13, 15, 6)      # create a vector to evaluate

ifelse(vector < 10, "A", "B")  # give the conditions: if inferior to 10, return A, if not, return B

vector2 <- c("What am I?", "A", "B", "C", "D")

case_when(vector2 == "What am I?" ~ "I am the walrus", # assign the new value with a tilde ~
          vector2 %in% c("A", "B") ~ "goo",
          vector2 == "C" ~ "ga",
          vector2 == "D" ~ "joob")


unique(trees$LatinName)  # Shows all the species names

# Changing factor levels or create categorical variables---- 

# Create a new genus column (categorical variable?) using mutate()

trees.genus <- trees %>%
  mutate(Genus = case_when(               # creates the genus column and specifies conditions
    grepl("Acer", LatinName) ~ "Acer",    # grepl function looks for patterns in the data and specifies what to return for each genus
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



# Finding genus names (same outcome) by using the separate() function to split the column  
# into several new ones (with words making up species names) keeping only the first (Latin Name)
# Create two new columns in a vector (genus name and species name)
# "sep" refers to the separator
# remove = FALSE means that we want to keep the original column LatinName in the data frame

trees.genus.2 <- trees %>% 
  tidyr::separate(LatinName, c("Genus", "Species"), sep = " ", remove = FALSE) %>%  
  dplyr::select(-Species)



# Another example of reclassifying a factor by creating new categorical variable in this case height

trees.genus <- trees.genus %>%   # overwriting our data frame 
  mutate(Height.cat =   # creating our new column
           case_when(Height %in% c("Up to 5 meters", "5 to 10 meters") ~ "Short",
                     Height %in% c("10 to 15 meters", "15 to 20 meters") ~ "Medium",
                     Height == "20 to 25 meters" ~ "Tall")
  )



# Reordering a factor's levels

levels(trees.genus$Height.cat)  # shows the different factor levels (height) in their default order

trees.genus$Height.cat <- factor(trees.genus$Height.cat,
                                 levels = c('Short', 'Medium', 'Tall'),   # whichever order you choose will be reflected in plots etc
                                 labels = c('SHORT', 'MEDIUM', 'TALL')    # Make sure you match the new names to the original levels!
)   

levels(trees.genus$Height.cat)  # a new order and new names for the levels

# Advanced piping and making graphs using ggplot2()----

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

# View the graphs before saving them
tree.plots$plots

# Saving the plots to file

# # '.$Genus': accesses the Genus name of the tree.plots object, each plot will bear a different name according to the tree genus
# ‘map-‘: a custom text bit that will be shared by all the plots. We’re drawing maps after all!
# ‘sep = ‘’’: we want all the previous bits to be pasted together with nothing separating them

tree.plots %>%              # the saving call within the do function
  do(., 
     ggsave(.$plots, filename = paste(getwd(), "/Output/", "map-", .$Genus, ".png", sep = ""), device = "png", height = 12, width = 16, units = "cm"))






