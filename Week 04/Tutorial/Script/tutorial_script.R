#Introduction----

# Tutorial 4 code # Analysing annual increments in stem growth,
#  measured on crowberry shrubs on a sand dune system

# Nicole Yap s1761850@ed.ac.uk 
# 14-10-2020
####################################################################

# Libraries and working dir ----

# Using RStudio project to handle relative path so no setwd needed
library(tidyr)
library(dplyr)

# Loading elongation data
elongation <- read.csv("Data/EmpetrumElongation.csv", header = TRUE)   

# Checking that data was imported correctly
head(elongation)   # first few observations
str(elongation)    # types of variables

# Accessing data with row and column numbers i.e the bad way---- 

#  Getting information out of object 
elongation$Indiv   # prints out all the ID codes in the dataset
length(unique(elongation$Indiv))   # returns the number of distinct shrubs in the data

# Getting value in the second row and fifth column
elongation[2,5]

# Getting all the info for row number 6
elongation[6, ]

# Mixing row number and the name of column to access info 
elongation[6, ]$Indiv   # returns the value in the column Indiv for the sixth observation

# Accessing data using logical operators i.e the good way----

# Accessing the values for Individual number 603
elongation[elongation$Indiv == 603, ]


# Subsetting with one condition

elongation[elongation$Zone < 4, ]    # returns only the data for zones 2-3
elongation[elongation$Zone <= 4, ]   # returns only the data for zones 2-3-4


# Returning only the data for zones 2-3-4 using another method 
elongation[!elongation$Zone >= 5, ]   # the ! means exclude


# Subsetting with two conditions
# return only data for zones 2 and 7
elongation[elongation$Zone == 2 | elongation$Zone == 7, ]    
# return data for shrubs in zone 2 whose ID numbers are between 300 and 400
elongation[elongation$Zone == 2 & elongation$Indiv %in% c(300:400), ]    

# Testing other vector sequence builders---- 

seq(300, 400, 10) # creating a sequence incrementing by 10

rep(c(1,2), 3) # repeating c(1,2) 3 times 

# Mixing the two methods above to get a sequence from 0 to 30, incrementing by 10, repeated 4 times. 
rep(seq(0, 30, 10), 4) 

## CHANGING VARIABLE NAMES AND VALUES IN A DATA FRAME----

# Creating a working copy of our object
elong2 <- elongation

# Using the names() function to access column names. 
# Using the function on the left side of the assign operator allows us to overwrite the names.

names(elong2)                 # returns the names of the columns

# Change Zone to zone: we call the 1st element of the names vector using brackets, and assign it a new value
names(elong2)[1] <- "zone"    

# Change Indiv to ID: we call the 2nd element and assign it the desired value
names(elong2)[2] <- "ID"      

# Two ways to change mistake in data: value 5.1 for individual 373 in year 2008 should be 5.7

# Option 1: Using row and column number is compact but not recommended 
elong2[1,4] <- 5.7

#Option 2: Using logical conditions for control so that code will run even if the observation moves in the dataset.
elong2[elong2$ID == 373, ]$X2008 <- 5.7 

## SPECIFYING VARIABLE CLASSES AND CREATING A FACTOR----

# Checking the classes
str(elong2)

# Setting the zone column as an integer grouping factor. 


elong2$zone <- as.factor(elong2$zone)        # converting and overwriting original class
str(elong2)                                  # now zone is a factor with 6 levels


# convert and overwrite original class from a plain column to a grouping factor
elong2$zone <- as.factor(elong2$zone)       
str(elong2)                                  # now zone is a factor with 6 levels

## CHANGING A FACTOR'S LEVELS

levels(elong2$zone)  # shows the different factor levels

# overwrite the original integer levels 2-7 with new, character names
levels(elong2$zone) <- c("A", "B", "C", "D", "E", "F")   

# TIDY DATASETS ----

# Gather in this order: data frame, key- what I'm gathering, value- what number represent e.g length 
# Here we want the lengths (value) to be gathered by year (key) 

elongation_long <- gather(elongation, Year, Length,                          
                          c(X2007, X2008, X2009, X2010, X2011, X2012))  # specify which columns to gather



# spread() is the inverse function, going from long to wide format
elongation_wide <- spread(elongation_long, Year, Length) 

# Can also use gather with column number instead of names (i.e gather colums 3 to 8)
elongation_long2 <- gather(elongation, Year, Length, c(3:8))

# Making boxplot for annual growth of Empetrum hermaphroditum using long format dataset----

boxplot(Length ~ Year, data = elongation_long, 
        xlab = "Year", ylab = "Elongation (cm)", 
        main = "Annual growth of Empetrum hermaphroditum")

# Exploring dplyr functions----

# Changing the names of the columns by getting rid of capital letters and overwriting data frame
elongation_long <- rename(elongation_long, zone = Zone, indiv = Indiv, year = Year, length = Length)  

# Previousing we changed the names using the base R names() function
names(elongation_long) <- c("zone", "indiv", "year", "length")

# Using dplyr functions to filter observations----

# Filtering observations from zones 2 and 3 only, and from years 2009 to 2011
# Use multiple different conditions separated by commas
elong_subset <- filter(elongation_long, zone %in% c(2, 3), year %in% c("X2009", "X2010", "X2011")) 

# For comparison, the base R equivalent would be (not assigned to an object here):
elongation_long[elongation_long$zone %in% c(2,3) & elongation_long$year %in% c("X2009", "X2010", "X2011"), ]

# Selecting columns of interest using dplyr select() function----

# Let's ignore the zone column just as an example

elong_no.zone <- dplyr::select(elongation_long, indiv, year, length) # Option 1 list columns of interest
elong_no.zone <- dplyr::select(elongation_long, -zone) # Option 2: the minus sign removes the column

# For comparison, the base R equivalent would be (not assigned to an object here):
elongation_long[ , -1]  # removes first column

# select() lets you rename and reorder columns easily
elong_no.zone <- dplyr::select(elongation_long, Year = year, Shrub.ID = indiv, Growth = length)

# Creating a new column using mutate() function----
# Adding data from multiple years to show total growth in new column
elong_total <- mutate(elongation, total.growth = X2007 + X2008 + X2009 + X2010 + X2011 + X2012)



# GROUP DATA to get the same result as above section using long format dataset----

elong_grouped <- group_by(elongation_long, indiv)   # grouping our dataset by individual

# SUMMARISING OUR DATA

# The first summary corresponds to the sum of all growth increments in the dataset (all individuals and years). 
# The second one gives us a breakdown of total growth per individual, our grouping variable
summary1 <- summarise(elongation_long, total.growth = sum(length)) 
summary2 <- summarise(elong_grouped, total.growth = sum(length))

#Using summary function to calculate various statistics
summary3 <- summarise(elong_grouped, total.growth = sum(length),
                      mean.growth = mean(length),
                      sd.growth = sd(length))

# Using join() to join datasets with shared attributes---- 

# Load the treatments associated with each individual

treatments <- read.csv("Data/EmpetrumTreatments.csv", header = TRUE, sep = ";")
head(treatments)

# Join the two data frames by ID code. 
# The column names are spelled differently, so we need to tell the function which columns represent a match.
#  We have two columns that contain the same information in both datasets: zone and individual ID.

# Option 1  using left_join()
experiment <- left_join(elongation_long, treatments, by = c("indiv" = "Indiv", "zone" = "Zone"))

# Option 2 using merge() which gives us the same result 
experiment2 <- merge(elongation_long, treatments, by.x = c("zone", "indiv"), by.y = c("Zone", "Indiv"))  

# Creating a boxplot from merged datasets to see if treatment affects growth----

boxplot(length ~ Treatment, data = experiment)








 

