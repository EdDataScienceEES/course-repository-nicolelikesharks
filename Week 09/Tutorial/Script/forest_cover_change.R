#Group 4 - Pink lake forest loss

#Libraries----
library(tidyverse)

#Loading the data----
forest_change <- read.csv("data_group_04/forest_change_Pink_Lake.csv")
str(forest_change)

#Putting data into long-form----
forest_change <- gather(forest_change, type, percentage, c(30:62))

# Creating barplot visualizing forest change as % of total park area
(forest_barplot <- ggplot(forest_change, aes(x = NAME, y = sum/GIS_AREA, colour = type, 
                                             fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = NULL, y = "Forest change (% of park area)\n") +
    # Expanding the scale removes the emtpy space below the bars
    scale_y_continuous(expand = c(0, 0)) +
    theme(text = element_text(size = 16),  # makes font size larger
          legend.position = c(0.1, 0.85),  # changes the placement of the legend
          legend.title = element_blank(),  # gets rid of the legend title
          legend.background = element_rect(color = "black", 
                                           fill = "transparent",   # removes the white background behind the legend
                                           linetype = "blank")))


# Saving output

ggsave(forest_barplot, filename = "Output/pink_lake_barplot.png",
       height = 5, width = 7)

