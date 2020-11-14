# Introduction---- 
# Tutorial on Beautiful and Informative Data Visualisation
# Nicole Yap s1761850@ed.ac.uk 
# 27-10-2020 

# Libraries and loading data---- 

library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(gridExtra)

# Import data from the Living Planet Index - population trends of vertebrate species from 1970 to 2014
LPI <- read.csv("Data/LPIdata_CC.csv")

# Reshaping data into long format and manipulating data for ease----
# By adding 9:53, we select columns 9 to 53, the ones for the different years of monitoring
LPI2 <- gather(LPI, "year", "abundance", 9:53)
View(LPI2)

# Transforming years from characters back into numbers using pares_number()
LPI2$year <- parse_number(LPI2$year)

# Checking if the variables have stayed how we want them using the str() function
str(LPI2)

# Transforming Abundance from character variable to numeric 
LPI2$abundance <- as.numeric(LPI2$abundance)

# Checking list of Common Names to choose one
unique(LPI2$`Common.Name`)

# Filtering out records for chosen species i.e Tiger Shark! yay
shark <- filter(LPI2, Common.Name == "Tiger shark")
head(shark)

# Getting rid of the empty rows using na.omit()
shark <- na.omit(shark)

# Visualising data----

# With base R graphics
base_hist <- hist(shark$abundance)

# Using ggplot2: creating graph with no brackets
shark_hist <- ggplot(shark, aes(x = abundance))  +
  geom_histogram() 

# Calling the object to display it in the plot viewer
shark_hist

# With brackets: Create and display the graph at the same time
(shark_hist <- ggplot(shark, aes(x = abundance))  +
    geom_histogram())


# Another way to check whether your data is normally distributed, is to either create density plots using package ggpubr and command ggdensity()
## OR use functions qqnorm() and qqline()

# Making our graph look pretty with ggplot2() 
(shark_hist <- ggplot(shark, aes(x = abundance)) +                
    geom_histogram(binwidth = 0.3, colour = "#EBA338", fill = "#8CC2BC") +    # Changing the binwidth and colours
    geom_vline(aes(xintercept = mean(abundance)),                       # Adding a line for mean abundance
               colour = "red", linetype = "dashed", size=1) +           # Changing the look of the line
    theme_bw() +                                                      # Changing the theme to get rid of the grey background
    ylab("Count\n") +                                                   # Changing the text of the y axis label
    xlab("\nTiger shark abundance")  +                              # \n adds a blank line between axis and text
    theme(axis.text = element_text(size = 12),                          # Changing font size of axis labels and title
          axis.title.x = element_text(size = 14, face = "plain"),       # face="plain" is the default, you can change it to italic, bold, etc. 
          panel.grid = element_blank(),                                 # Removing the grey grid lines
          plot.margin = unit(c(1,1,1,1), units = , "cm")))              # Putting a 1 cm margin around the plot

# We can see from the histogram that the data are very skewed - a typical distribution of count abundance data?? 

# Choosing custom colours
c("#8CC2BC", "#EBA338", "#A7CF6C")

# Filtering the data to get records only from South Africa and Australia using the `filter()` function from the `dplyr` package
sharkSAAU <- filter(shark, Country.list %in% c("South Africa", "Australia"))


# Using default ggplot2 to create scatterplot showing how tiger shark populations have changed between 1970 and 2017 in Australia and in S. Africa.
# Linking colour to a factor inside aes() ensures that the points' colour will vary according to the factor levels
(shark_scatter <- ggplot(sharkSAAU, aes(x = year, y = abundance, colour = Country.list)) +  
    geom_point())

# # Beautifying scatterplot and adding linear model fit line

(shark_scatter <- ggplot(sharkSAAU, aes (x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 1.5) +                                               # Changing point size
    geom_smooth(method = "lm", aes(fill = Country.list)) +               # Adding linear model fit, colour-code by country
    theme_bw() +
    scale_fill_manual(values = c("#8CC2BC", "#EBA338")) +                # Adding custom colours for solid geoms (ribbon)
    scale_colour_manual(values = c("#8CC2BC", "#EBA338"),                # Adding custom colours for lines and points
                        labels = c("Australia", "South Africa")) +                # Adding labels for the legend
    ylab("Tiger shark abundance\n") +                             
    xlab("\nYear")  +
    theme(axis.text.x = element_text(size = 11, angle = 60, vjust = 1, hjust = 1),     # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),                                   # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),                 # Adding a 1cm margin around the plot
          legend.text = element_text(size = 10, face = "bold"),         # Setting the font for the legend text
          legend.title = element_blank(),                                 # Removing the legend title
          legend.position = c(0.68, 0.8)))                                 # Setting legend position - 0 is left/bottom, 1 is top/righ
                                


# Creating boxplot to examine whether shark abundance differs between South Africa and Australia

(shark_boxplot <- ggplot(sharkSAAU, aes('Country list', abundance)) + geom_boxplot())

# Beautifying boxplot by removing background grid lines and legend 

(shark_boxplot <- ggplot(sharkSAAU, aes(Country.list, abundance)) + 
    geom_boxplot(aes(fill = Country.list)) +
    theme_bw() +
    scale_fill_manual(values = c("#8CC2BC", "#A7CF6C")) +               # Adding custom colours
    scale_colour_manual(values = c("8CC2BC", "#A7CF6C")) +             # Adding custom colours
    ylab("Tiger shark abundance\n") +                             
    xlab("\nCountry")  +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                     
          panel.grid = element_blank(),                                 # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),               # Adding a margin
          legend.position = "none"))                                    # Removing legend - not needed with only 2 factors


# Barplot to compare species richness of a few European countries

# Calculating species richness using pipes %>% from the dplyr package
richness <- LPI2 %>% filter (Country.list %in% c("United Kingdom", "Romania", "France", "Spain", "Italy")) %>%
  group_by(Country.list) %>%
  mutate(richness = (length(unique(Common.Name)))) # create new column based on how many unique common names (or species) there are in each country 

# Plotting the species richness
(richness_barplot <- ggplot(richness, aes(x = Country.list, y = richness)) +
    geom_bar(position = position_dodge(), stat = "identity", colour = "black", fill = "#00868B") +
    theme_bw() +
    ylab("Species richness\n") +                             
    xlab("Country")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),  # Angled labels, so text doesn't overlap
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                      
          panel.grid = element_blank(),                                          
          plot.margin = unit(c(1,1,1,1), units = , "cm")))

# Plot the shark population change for all countries on the same graph! In this case not that cluttered... 
(shark_scatter_all <- ggplot(shark, aes (x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 1) +                                               # Changing point size
    geom_smooth(method = "lm", aes(fill = Country.list)) +               # Adding linear model fit, colour-code by country
    theme_bw() +
    ylab("Tiger shark abundance\n") +                             
    xlab("\nYear")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),     # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),                                   # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),                 # Adding a 1cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),         # Setting the font for the legend text
          legend.title = element_blank(),                                 # Removing the legend title
          legend.position = "right"))   

# By using facet_wrap() and adding a facetting layer we split the data in multiple facets representing the different countries.

# Plot the population change for countries individually
(shark_scatter_facets <- ggplot(shark, aes (x = year, y = abundance, colour = Country.list)) +
    geom_point(size = 2) +                                               # Changing point size
    geom_smooth(method = "lm", aes(fill = Country.list)) +               # Adding linear model fit, colour-code by country
    facet_wrap(~ Country.list, scales = "free_y") +                      # THIS LINE CREATES THE FACETTING
    theme_bw() +
    ylab("Tiger shark abundance\n") +                             
    xlab("\nYear")  +
    theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1),     # making the years at a bit of an angle
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"),                        
          panel.grid = element_blank(),                                   # Removing the background grid lines               
          plot.margin = unit(c(1,1,1,1), units = , "cm"),                 # Adding a 1cm margin around the plot
          legend.text = element_text(size = 12, face = "italic"),         # Setting the font for the legend text
          legend.title = element_blank(),                                 # Removing the legend title
          legend.position = "right"))   

# Attempting to arrange multiple figures together to create a panel

grid.arrange(shark_hist, shark_scatter, shark_boxplot, ncol = 1)

# But this doesn't look right -graphs are too stretched, the legend and text are all messed up, the white margins are too big

# Fixing the problems - adding ylab() again overrides the previous settings

(panel <- grid.arrange(
  shark_hist + ggtitle("(a)") + ylab("Count") + xlab("Abundance") +   # adding labels to the different plots
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")),
  
  shark_boxplot + ggtitle("(b)") + ylab("Abundance") + xlab("Country") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")),
  
  shark_scatter + ggtitle("(c)") + ylab("Abundance") + xlab("Year") +
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), units = , "cm")) +
    theme(legend.text = element_text(size = 10, face = "italic"),     
          legend.title = element_blank(),                                   
          legend.position = c(0.8, 0.87)), # changing the legend position so that it fits within the panel
  
  ncol = 1, heights = c(1, 1, 2))) # ncol determines how many columns you have

# Saving output panel into wd()---- 

ggsave(panel, file = "Output/shark_panel2.png", width = 5, height = 12) 
