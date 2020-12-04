# Introduction---- 
# Tutorial on efficient and beautiful data visualization
# Nicole Yap s1761850@ed.ac.uk
# 28-10-2020

# Libraries---- 

library(tidyverse)
library(ggthemes)  # for a mapping theme
library(ggalt)  # for custom map projections
library(ggrepel)  # for annotations
library(viridis)  # for nice colours

# Loading and checking data----

# Load data - Site coordinates and plant records available from
# the Long Term Ecological Research Network
# https://lternet.edu and the Niwot Ridge site more specifically

lter <- read.csv("Data/lter.csv")
niwot_plant_exp <- read.csv("Data/niwot_plant_exp.csv")


# Making maps----


# Get the shape of North America
north_america <- map_data("world", region = c("USA", "Canada"))

# Exclude Hawaii if you want to
north_america <- north_america[!(north_america$subregion %in% "Hawaii"),]

# A very basic map
(lter_map1 <- ggplot() +
    geom_map(map = north_america, data = north_america,
             aes(long, lat, map_id = region), 
             color = "gray80", fill = "gray80", size = 0.3) +
    # Add points for the site locations
    geom_point(data = lter, 
               aes(x = long, y = lat)))

# You can ignore this warning message, it's cause we have forced
# specific lat and long columns onto geom_map()
# Warning: Ignoring unknown aesthetics: x, y

# Saving this ugly map

ggsave(lter_map1, filename = "Output/ugly_map1.png",
       height = 5, width = 8)  # the units by default are in inches

# Making better maps!----
# Adding colours to indicate elevation at each site

(lter_map2 <- ggplot() +
    geom_map(map = north_america, data = north_america,
             aes(long, lat, map_id = region), 
             color = "gray80", fill = "gray80", size = 0.3) +
    geom_point(data = lter, 
               aes(x = long, y = lat, fill = ele),
               # when you set the fill or colour to vary depending on a variable
               # you put that (e.g., fill = ele) inside the aes() call
               # when you want to set a specific colour (e.g., colour = "grey30"),
               # that goes outside of the aes() call
               alpha = 0.8, size = 4, colour = "grey30",
               shape = 21))

ggsave(lter_map2, filename = "Output/elevation_map2.png",
       height = 5, width = 8)

# Improving map projection by swapping default projection 

(lter_map3 <- ggplot() +
    geom_map(map = north_america, data = north_america,
             aes(long, lat, map_id = region), 
             color = "gray80", fill = "gray80", size = 0.3) +
    # you can change the projection here
    # coord_proj("+proj=wintri") +
    # the wintri one above is good for the whole world, the one below for just North America
    coord_proj(paste0("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96",
                      " +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")) +
    geom_point(data = lter, 
               aes(x = long, y = lat, fill = ele),
               alpha = 0.8, size = 4, colour = "grey30",
               shape = 21))

# don't need to worry about the warning messages
# that's just cause we've overwritten the default projection

ggsave(lter_map3, filename = "Output/improved_projn_map3.png",
       height = 5, width = 8)

# Zoomy zoom on the map 

(lter_map4 <- ggplot() +
        geom_map(map = north_america, data = north_america,
                 aes(long, lat, map_id = region), 
                 color = "gray80", fill = "gray80", size = 0.3) +
        coord_proj(paste0("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96",
                          " +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"),
                   # zooming in by setting specific coordinates
                   ylim = c(25, 80), xlim = c(-175, -50)) +
        geom_point(data = lter, 
                   aes(x = long, y = lat, fill = ele),
                   alpha = 0.8, size = 4, colour = "grey30",
                   shape = 21))

ggsave(lter_map4, filename = "Output/zoomed_map4.png",
       height = 5, width = 8)

# Cleaning map up further: Removing grid and axis labels etc. 

(lter_map5 <- ggplot() +
        geom_map(map = north_america, data = north_america,
                 aes(long, lat, map_id = region), 
                 color = "gray80", fill = "gray80", size = 0.3) +
        coord_proj(paste0("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96",
                          " +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"),
                   ylim = c(25, 80), xlim = c(-175, -50)) +
        geom_point(data = lter, 
                   aes(x = long, y = lat, fill = ele),
                   alpha = 0.8, size = 4, colour = "grey30",
                   shape = 21) +
        # Adding a clean map theme
        theme_map() +
        # Putting the legend at the bottom
        theme(legend.position = "bottom"))

ggsave(lter_map5, filename = "Output/cleaner_map5.png",
       height = 5, width = 8)

# Annotating points with what the sites are for detail 

(lter_map6 <- ggplot() +
        geom_map(map = north_america, data = north_america,
                 aes(long, lat, map_id = region), 
                 color = "gray80", fill = "gray80", size = 0.3) +
        coord_proj(paste0("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96",
                          " +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"),
                   ylim = c(25, 80), xlim = c(-175, -50)) +
        geom_point(data = lter, 
                   aes(x = long, y = lat, fill = ele),
                   alpha = 0.8, size = 4, colour = "grey30",
                   shape = 21) +
        theme_map() +
        theme(legend.position = "bottom") +
        # Adding point annotations with the site name
        geom_label_repel(data = lter,
                         aes(x = long, y = lat,
                             label = site),
                         # Setting the positions of the labels
                         box.padding = 1, size = 4, nudge_x = 1, nudge_y = 1))

ggsave(lter_map6, filename = "Output/annotated_map6.png",
       height = 5, width = 8)


# Narrowing down to Niwot Ridge site 

(lter_map7 <- ggplot() +
        geom_map(map = north_america, data = north_america,
                 aes(long, lat, map_id = region), 
                 color = "gray80", fill = "gray80", size = 0.3) +
        coord_proj(paste0("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96",
                          " +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"),
                   ylim = c(25, 80), xlim = c(-175, -50)) +
        geom_point(data = lter, 
                   aes(x = long, y = lat, fill = ele),
                   alpha = 0.8, size = 4, colour = "grey30",
                   shape = 21) +
        theme_map() +
        theme(legend.position = "bottom") +
        geom_label_repel(data = subset(lter, ele > 2000),
                         aes(x = long, y = lat,
                             label = site),
                         box.padding = 1, size = 4, nudge_x = 1, nudge_y = 12))

ggsave(lter_map7, filename = "Output/niwot_map7.png",
       height = 5, width = 8)


# Adding text detail about site

(lter_map8 <- ggplot() +
        geom_map(map = north_america, data = north_america,
                 aes(long, lat, map_id = region), 
                 color = "gray80", fill = "gray80", size = 0.3) +
        coord_proj(paste0("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96",
                          " +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"),
                   ylim = c(25, 80), xlim = c(-175, -50)) +
        geom_point(data = lter, 
                   aes(x = long, y = lat, fill = ele),
                   alpha = 0.8, size = 4, colour = "grey30",
                   shape = 21) +
        theme_map() +
        theme(legend.position = "bottom") +
        geom_label_repel(data = subset(lter, ele > 2000),
                         aes(x = long, y = lat,
                             label = site),
                         box.padding = 1, size = 4, nudge_x = 1, nudge_y = 12) +
        labs(fill = "Elevation (m)") +
        annotate("text", x = -150, y = 35, colour = "#553c7f",
                 label = "At 3528 m above sea level,\nNiwot Ridge is\nthe highest LTER site.",
                 size = 4.5, fontface = "bold") +
        scale_fill_viridis(option = "magma", direction = -1, begin = 0.2))

ggsave(lter_map8, filename = "Output/niwot_final_map.png",
       height = 5, width = 8)


# Visualizing distributions----

# Creating theme


# Setting a custom ggplot2 function
# This function makes a pretty ggplot theme
# This function takes no arguments 
# meaning that you always have just niwot_theme() and not niwot_theme(something else here)

theme_niwot <- function(){
    theme_bw() +
        theme(text = element_text(family = "Helvetica Light"),
              axis.text = element_text(size = 16), 
              axis.title = element_text(size = 18),
              axis.line.x = element_line(color="black"), 
              axis.line.y = element_line(color="black"),
              panel.border = element_blank(),
              panel.grid.major.x = element_blank(),                                          
              panel.grid.minor.x = element_blank(),
              panel.grid.minor.y = element_blank(),
              panel.grid.major.y = element_blank(),  
              plot.margin = unit(c(1, 1, 1, 1), units = , "cm"),
              plot.title = element_text(size = 18, vjust = 1, hjust = 0),
              legend.text = element_text(size = 12),          
              legend.title = element_blank(),                              
              legend.position = c(0.95, 0.15), 
              legend.key = element_blank(),
              legend.background = element_rect(color = "black", 
                                               fill = "transparent", 
                                               size = 2, linetype = "blank"))
}


# Calculate species richness per plot per year using pipes
niwot_richness <- niwot_plant_exp %>% group_by(plot_num, year) %>%
    mutate(richness = length(unique(USDA_Scientific_Name))) %>% ungroup()

# Visualize how species richness varies across fertilisation treatments.
(distributions1 <- ggplot(niwot_richness, aes(x = fert, y = richness)) +
        geom_violin())

ggsave(distributions1, filename = "Output/distributions1.png",
       height = 5, width = 5)

# Adding theme and colour 

(distributions2 <- ggplot(niwot_richness, aes(x = fert, y = richness)) +
        geom_violin(aes(fill = fert, colour = fert), alpha = 0.5) +
        # alpha controls the opacity
        theme_niwot())

ggsave(distributions2, filename = "Output/colour_distbn2.png",
       height = 5, width = 5)

# Adding violins with boxplots

(distributions3 <- ggplot(niwot_richness, aes(x = fert, y = richness)) +
        geom_violin(aes(fill = fert, colour = fert), alpha = 0.5) +
        geom_boxplot(aes(colour = fert), width = 0.2) +
        theme_niwot())

ggsave(distributions3, filename = "Output/violin_distrbn3.png",
       height = 5, width = 5)

# Adding data points

(distributions4 <- ggplot(niwot_richness, aes(x = fert, y = richness)) +
        geom_violin(aes(fill = fert, colour = fert), alpha = 0.5) +
        geom_jitter(aes(colour = fert), position = position_jitter(0.1), 
                    alpha = 0.3) +
        theme_niwot())

ggsave(distributions4, filename = "Output/point_distributions4.png",
       height = 5, width = 5)


# Creating raincloud plots---- 


# We will use a function by Ben Marwick
# This code loads the function in the working environment
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

# Now we can make the plot!
(distributions5 <- 
        ggplot(data = niwot_richness, 
               aes(x = reorder(fert, desc(richness)), y = richness, fill = fert)) +
        # The half violins
        geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
        # The points
        geom_point(aes(y = richness, color = fert), 
                   position = position_jitter(width = 0.15), size = 1, alpha = 0.1) +
        # The boxplots
        geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8) +
        # \n adds a new line which creates some space between the axis and axis title
        labs(y = "Species richness\n", x = NULL) +
        # Removing legends
        guides(fill = FALSE, color = FALSE) +
        # Setting the limits of the y axis
        scale_y_continuous(limits = c(0, 30)) +
        # Picking nicer colours
        scale_fill_manual(values = c("#5A4A6F", "#E47250",  "#EBB261", "#9D5A6C")) +
        scale_colour_manual(values = c("#5A4A6F", "#E47250",  "#EBB261", "#9D5A6C")) +
        theme_niwot())

ggsave(distributions5, filename = "Output/combo_rain_violin5.png",
       height = 5, width = 5)

# Flipping x and y axis 

(distributions6 <- 
        ggplot(data = niwot_richness, 
               aes(x = reorder(fert, desc(richness)), y = richness, fill = fert)) +
        geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
        geom_point(aes(y = richness, color = fert), 
                   position = position_jitter(width = 0.15), size = 1, alpha = 0.1) +
        geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8) +
        labs(y = "\nSpecies richness", x = NULL) +
        guides(fill = FALSE, color = FALSE) +
        scale_y_continuous(limits = c(0, 30)) +
        scale_fill_manual(values = c("#5A4A6F", "#E47250",  "#EBB261", "#9D5A6C")) +
        scale_colour_manual(values = c("#5A4A6F", "#E47250",  "#EBB261", "#9D5A6C")) +
        coord_flip() +
        theme_niwot())

ggsave(distributions6, filename = "Output/flip_rain_violin6.png",
       height = 5, width = 5)


# Using case_when() combined with mutate() to create new variables based  on one 
## or more conditions from other variables 

# Create new columns based on a combo of conditions using case_when()
# A fictional example

alpine_magic <- niwot_richness %>% mutate(fairy_dust = case_when(fert == "PP" & hits > 5 ~ "Blue fairy dust",
                                                                 fert == "CC" & hits > 15 ~ "The ultimate fairy dust"))

(distributions_magic <- 
        ggplot(data = alpine_magic, 
               aes(x = reorder(fairy_dust, desc(richness)), y = richness, fill = fairy_dust)) +
        geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
        geom_point(aes(y = richness, color = fairy_dust), 
                   position = position_jitter(width = 0.15), size = 1, alpha = 0.1) +
        geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8) +
        labs(y = "\nSpecies richness", x = NULL) +
        guides(fill = FALSE, color = FALSE) +
        scale_y_continuous(limits = c(0, 30)) +
        scale_fill_manual(values = c("turquoise4", "magenta4")) +
        scale_colour_manual(values = c("turquoise4", "magenta4")) +
        coord_flip() +
        theme_niwot())

ggsave(distributions_magic, filename = "Output/new_var_magic1.png",
       height = 5, width = 5)

# Using drop_na() to specify which specific column should be the evaluator, 
## and drop missing values

alpine_magic_only <- alpine_magic %>% drop_na(fairy_dust) # here, fairy dust is the evaluator

(distributions_magic2 <- 
        ggplot(data = alpine_magic_only, 
               aes(x = reorder(fairy_dust, desc(richness)), y = richness, fill = fairy_dust)) +
        geom_flat_violin(position = position_nudge(x = 0.2, y = 0), alpha = 0.8) +
        geom_point(aes(y = richness, color = fairy_dust), 
                   position = position_jitter(width = 0.15), size = 1, alpha = 0.1) +
        geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0.8) +
        labs(y = "\nSpecies richness", x = NULL) +
        guides(fill = FALSE, color = FALSE) +
        scale_y_continuous(limits = c(0, 30)) +
        scale_fill_manual(values = c("turquoise4", "magenta4")) +
        scale_colour_manual(values = c("turquoise4", "magenta4")) +
        coord_flip() +
        theme_niwot())

ggsave(distributions_magic2, filename = "Output/filtered_na_magic2.png",
       height = 5, width = 5)


# Make, customise and annotate histograms---- 

# Using the tally() function combned with group_by() creates summary of how many 
## observations there are in different categories in my data

# Calculate number of data records per plot per year

observations <- niwot_plant_exp %>% group_by(USDA_Scientific_Name) %>%
    tally() %>% arrange(desc(n))  # rearanging the data frame so that the most common species are first


# Filtering for plant species records from the Carex family 

# Filtering out just Carex species
carex <- niwot_plant_exp %>%
    filter(str_detect(USDA_Scientific_Name, pattern = "Carex"))

# Making histogram for Carex distribution 

(histogram1 <- ggplot(carex, aes(x = hits)) +
        geom_histogram())

ggsave(histogram1, filename = "Output/hist_carex.png",
       height = 5, width = 5)

# Making prettier histogram for Carex species

(histogram2 <- ggplot(carex, aes(x = hits)) +
        geom_histogram(alpha = 0.6, 
                       breaks = seq(0, 100, by = 3),
                       # Choosing a Carex-like colour
                       fill = "seagreen4") +
        theme_niwot())

ggsave(histogram2, filename = "Output/colour_hist2.png",
       height = 5, width = 5)

# Remove empty space 

(histogram3 <- ggplot(carex, aes(x = hits)) +
        geom_histogram(alpha = 0.6, 
                       breaks = seq(0, 100, by = 3),
                       fill = "palegreen4") +
        theme_niwot() +
        scale_y_continuous(limits = c(0, 100), expand = expand_scale(mult = c(0, 0.1))))
# the final line of code removes the empty blank space below the bars)

ggsave(histogram3, filename = "Output/remove_sp_hist3.png",
       height = 5, width = 5)

# # Adding an outline around the whole histogram
h <- hist(carex$hits, breaks = seq(0, 100, by = 3), plot = FALSE)
d1 <- data.frame(x = h$breaks, y = c(h$counts, NA))  
d1 <- rbind(c(0, 0), d1)

# Moving data frame from main ggplot() call to specific part of graph where we 
## want to use dataset 

(histogram4 <- ggplot(carex, aes(x = hits)) +
        geom_histogram(alpha = 0.6, 
                       breaks = seq(0, 100, by = 3),
                       fill = "palegreen4") +
        theme_niwot() +
        scale_y_continuous(limits = c(0, 100), expand = expand_scale(mult = c(0, 0.1))) +
        # Adding the outline
        geom_step(data = d1, aes(x = x, y = y),
                  stat = "identity", colour = "seagreen4"))

summary(d1) # ignore the warning message because some values don't have bars
            ## thus there are missing "steps" along the geom_step path

ggsave(histogram4, filename = "Output/diff_datafr_same_hist4.png",
       height = 5, width = 5)

# Adding line for mean number of hits plus annotations for readability 

(histogram5 <- ggplot(carex, aes(x = hits)) +
        geom_histogram(alpha = 0.6, 
                       breaks = seq(0, 100, by = 3),
                       fill = "palegreen4") +
        theme_niwot() +
        scale_y_continuous(limits = c(0, 100), expand = expand_scale(mult = c(0, 0.1))) +
        geom_step(data = d1, aes(x = x, y = y),
                  stat = "identity", colour = "palegreen4") +
        geom_vline(xintercept = mean(carex$hits), linetype = "dotted",
                   colour = "palegreen4", size = 1) +
        # Adding in a text allocation - the coordinates are based on the x and y axes
        annotate("text", x = 50, y = 50, label = "The mean number of\nCarex observations was 16.") +
        # "\n" creates a line break
        geom_curve(aes(x = 50, y = 60, xend = mean(carex$hits) + 2, yend = 60),
                   arrow = arrow(length = unit(0.07, "inch")), size = 0.7,
                   color = "grey30", curvature = 0.3) +
        labs(x = "\nObservation hits", y = "Count\n"))
# Similarly to the annotation, the curved line follows the plot's coordinates
# Have a go at changing the curve parameters to see what happens

ggsave(histogram5, filename = "Output/mean_hist5.png",
       height = 5, width = 5)
