# Introduction----

# Tidying up Bad_Script.R

# Nicole Yap s1761850@ed.ac.uk
# 07-10-2020

# Libraries----

library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(RCurl)

# Setting Working Directory----
setwd("C:/Users/nicol/Documents/Data Science Course/Week 2 Tutorial/bad script")

# Setting theme for ggplot2----
LPI.theme <- function() {
    theme_bw() + 
        theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 1, hjust = 1), 
                            axis.text.y = element_text(size = 12), 
                            axis.title.x = element_text(size = 14, face = "plain"), 
                            axis.title.y = element_text(size = 14, 
                            face = "plain"), panel.grid.major.x = element_blank(),
                            panel.grid.minor.x = element_blank(), 
                            panel.grid.minor.y = element_blank(), 
                            panel.grid.major.y = element_blank(),
                            plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm"),
                            plot.title = element_text(size = 20, vjust = 1, hjust = 0.5), 
                            legend.text = element_text(size = 12, face = "italic"), 
                            legend.title = element_blank(),
                            legend.position = c(0.9, 0.9))
}



# Import data from .csv----
LPI <- read.csv("LPIdata_CC.csv")

#Tidying script----


# Abundance records in one column 
LPI_column <- gather(LPI, "year", "abundance", 9:53)

# Convert years to numeric 
LPI_column$year <- parse_number(LPI_column$year)

# Making names lowercase
names(LPI_column)
names(LPI_column) <- tolower(names(LPI_column)) 

# Convert abundance to numeric
LPI_column$abundance <- as.numeric(LPI_column$abundance)

# Pipelines 
lpiBiomes <- LPI_column %>% 
    group_by(biome) %>% 
    summarise(Pop. = n())

# Check data frame
head(LPI_biome_summ)


# tbh don't know what this is for should probs delete it but shrugs
levels(LPI_column$biome)

#bar plot

type = "bar"
plot <- ggplot(LPI_column, aes(biome, color = biome)) + {
    if (type == "bar") geom_bar() else geom_point(stat = "count")
    } + 
    LPI.theme() +
    ylab("Number of populations") +
    xlab("Biome") + 
    theme(legend.position = "none")


#point plot

type = "point"
plot <- ggplot(LPI_column, aes(biome, color = biome)) + {
    if (type == "bar") geom_bar() else geom_point(stat = "count")
        } + 
        LPI.theme() + 
        ylab("Number of populations") +
        xlab("Biome") +
        theme(legend.position = "none")


# Save plot to pdf----

pdf(file = "plot1.pdf", width = 13.33, height = 26.66)
ggplot(LPI_column, aes(biome, color = biome)) + {
    if (type == "bar") geom_bar() else geom_point(stat = "count")
    } + 
    LPI.theme() +
    ylab("Number of populations") + 
    xlab("Biome") +
    theme(legend.position = "none")

dev.off()



