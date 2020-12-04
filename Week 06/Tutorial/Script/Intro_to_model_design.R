# Introduction---- 
# Intro to model design tutorial   
# Nicole Yap s1761850@ed.ac.uk 
  
# Libraries---- 
library(dplyr)  # for data manipulation
library(ggplot2)  # for data visualisation
library(lme4)  # for models
library(sjPlot)  # to visualise model outputs
library(ggeffects)  # to visualise model predictions
library(MCMCglmm)  # for models
library(MCMCvis)  # to visualise model outputs
library(brms)  # for models
library(stargazer)  # for tables of model outputs

# Load data ----

toolik_plants <- read.csv("Data/toolik_plants.csv")

# Inspect data
head(toolik_plants)
  
# Checking what class of data we're dealing with 

str(toolik_plants)

# Converting plot numbers from numerical to categorical variables 

toolik_plants$Plot <- as.factor(as.character(toolik_plants$Plot))

# Get the unique site names
unique(toolik_plants$Site)
length(unique(toolik_plants$Site))

# Group the dataframe by Site to see the number of blocks per site
toolik_plants %>% group_by(Site) %>%
  summarise(block.n = length(unique(Block)))

# Get unique smaller plots for each plot 
unique(toolik_plants$Year)

# Using unique() and length() functions, we can count how many species there are in the dataset as a whole 
length(unique(toolik_plants$Species))

# Checking to see if they are all species and not other thingse.g poop,water, mushrooms
unique(toolik_plants$Species)

# Filtering out non-species records 

# We use ! to say that we want to exclude
# all records that meet the criteria

# We use %in% i.e piping as a shortcut - we are filtering by many criteria
# but they all refer to the same column: Species

toolik_plants <- toolik_plants %>%
  filter(!Species %in% c("Woody cover", "Tube",
                         "Hole", "Vole trail",
                         "removed", "vole turds",
                         "Mushrooms", "Water",
                         "Caribou poop", "Rocks",
                         "mushroom", "caribou poop",
                         "animal litter", "vole poop",
                         "Vole poop", "Unk?"))


# Checking number of species after filtering non-species out 
length(unique(toolik_plants$Species))


# Calculate species richness in each plot in each survey year
toolik_plants <- toolik_plants %>%
  group_by(Year, Site, Block, Plot) %>%
  mutate(Richness = length(unique(Species)))


# Making histogram of species richness 

(hist <- ggplot(toolik_plants, aes(x = Richness)) +
    geom_histogram() +
    theme_classic())

# Being detailed about types of numeric data i.e years need to be whole numbers, 
## plant cover needs to be a positive value between 0 and 1

(hist2 <- ggplot(toolik_plants, aes(x = Relative.Cover)) +
    geom_histogram() +
    theme_classic())


# General linear models---- 

# Model without any random effects
plant_m <- lm(Richness ~ I(Year-2007), data = toolik_plants)
summary(plant_m)

# Assumptions made: 
# 1. The data are normally distributed.
# 2.  The data points are independent of one another.
# 3. The relationship between the variables we are studying is actually linear.

# Our data is not normally distributed. They are integer counts (no. of species), 
## probably skewed to the left as most plots might not have a large number of species. 
# Therefore, poisson distribution might be more suitable instead of normal one. 

# Checking model convergence i.e whether or not model has worked---- 


# Checking residual vs preicted plot for linear model by using plot() function to 
## plot residuals versus fitted values, a Q-Q plot of standardised residuals, 
## a scale-location plot (square roots of standardiaed residuals versus fitted values) and a plot of residuals versus 
## leverage that adds bands corresponding to Cook’s distances of 0.5 and 1


plot(plant_m)

# Hierarchical models using lme4---- 

# Model with only site as a random effect. Does not incorporate the temporal replication in the data or
## the fact that there are plots within blocks within those sites.

# Transform the Year column - I(Year - 2007) so that the year 2008 becomes Year 1 - 
## Model will estimate richness across the first, second, etc., year from survey period

plant_m_plot <- lmer(Richness ~ I(Year-2007) + (1|Site), data = toolik_plants)
summary(plant_m_plot)
plot(plant_m_plot)  # Checking assumptions


# Accounting for different plots and blocks 

plant_m_plot2 <- lmer(Richness ~ I(Year-2007) + (1|Site/Block), data = toolik_plants)
summary(plant_m_plot2)

plant_m_plot3 <- lmer(Richness ~ I(Year-2007) + (1|Site/Block/Plot), data = toolik_plants)
summary(plant_m_plot3)

# Visualizing final model using sjPlot package after accounting for hierarchical structure of data----

# Set a clean theme for the graphs
set_theme(base = theme_bw() + 
            theme(panel.grid.major.x = element_blank(),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.minor.y = element_blank(),
                  panel.grid.major.y = element_blank(),
                  plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = , "cm")))

# Visualises random effects 
(re.effects <- plot_model(plant_m_plot3, type = "re", show.values = TRUE))
save_plot(filename = "Output/model_randeff.png",
          height = 11, width = 9)  # Save the plot for random effects


# To see the estimate for our fixed effect (default): Year
(fe.effects <- plot_model(plant_m_plot3, show.values = TRUE))
save_plot(filename = "Output/model_fixeff.png",
          height = 11, width = 9)  # Save plot for fixed effects


# Exploring plot that shows us random effects of the site 

plant_m_temp <- lmer(Richness ~ Mean.Temp + (1|Site/Block/Plot) + (1|Year),
                     data = toolik_plants)
summary(plant_m_temp)


# Random slopes VS random intercepts---- 

# Allow each plot to have it's own relationship with temperature 

plant_m_rs <- lmer(Richness ~ Mean.Temp + (Mean.Temp|Site/Block/Plot) + (1|Year),
                   data = toolik_plants)
summary(plant_m_rs) # not converging!

# Simplifying model structure to attempt solve problem (still doesn't work) 

plant_m_rs <- lmer(Richness ~ Mean.Temp + (Mean.Temp|Plot) + (1|Year),
                   data = toolik_plants)
summary(plant_m_rs) # not converging either

# Try with just plot random intercept and with random slopes to illustrate what random slope model looks like

plant_m_rs <- lmer(Richness ~ Mean.Temp + (Mean.Temp|Plot),
                   data = toolik_plants)
summary(plant_m_rs)

# Visualizing results of model and saving plots simultaneously

(plant.re.effects <- plot_model(plant_m_rs, type = "re", show.values = TRUE))
save_plot(filename = "Output/model_randeff2.png",
          height = 17, width = 15)

(plant.fe.effects <- plot_model(plant_m_rs, show.values = TRUE))
save_plot(filename = "Output/model_fixeff2.png",
          height = 14, width = 9)


# Visualizing model predictions using ggpredict()

# Calculate the overall predictions for the relationship between species richness and temperature

ggpredict(plant_m_rs, terms = c("Mean.Temp")) %>% plot()
save_plot(filename = "Output/model_temp_richness.png",
          height = 9, width = 9)

# Calculate the predictions for each plot, thus visualising the among-plot variation. 
## Has freely varying slopes and intercepts (i.e., they’re different for each plot).

ggpredict(plant_m_rs, terms = c("Mean.Temp", "Plot"), type = "re") %>% plot()
save_plot(filename = "Output/model_temp_richness_rs_ri.png", # random slope, random intercept
          height = 9, width = 9)


# Manually plot predictions to overcome problem i.e y axis doesn't start at 0, 
## making relationship seem stronger that it actually is 

# Overall predictions - note that we have specified just mean temperature as a term
predictions <- ggpredict(plant_m_rs, terms = c("Mean.Temp"))

(pred_plot1 <- ggplot(predictions, aes(x, predicted)) +
    geom_line() +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1) +
    scale_y_continuous(limits = c(0, 22)) +
    labs(x = "\nMean annual temperature", y = "Predicted species richness\n"))

ggsave(pred_plot1, filename = "Output/overall_predictions.png",
       height = 5, width = 5)


# Overall predictions taking into account random effect 

# Predictions for each grouping level (here plot which is a random effect)
# re stands for random effect
predictions_rs_ri <- ggpredict(plant_m_rs, terms = c("Mean.Temp", "Plot"), type = "re")

(pred_plot2 <- ggplot(predictions_rs_ri, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE)  +
    scale_y_continuous(limits = c(0, 22)) +
    labs(x = "\nMean annual temperature", y = "Predicted species richness\n"))

ggsave(pred_plot2, filename = "Output/ri_rs_predictions.png",
       height = 5, width = 5)


# Zooming in 

(pred_plot3 <- ggplot(predictions_rs_ri, aes(x = x, y = predicted, colour = group)) +
    stat_smooth(method = "lm", se = FALSE)  +
    labs(x = "\nMean annual temperature", y = "Predicted species richness\n"))

# In actual reports remember that axes should start at 0!
  ggsave(pred_plot3, filename = "Output/ri_rs_predictions_zoom.png",
       height = 5, width = 5)
  

# Exploring models in MCMCglmm----
  
# Build a more complex model, starting with a Site random effect 
  
  plant_mcmc <- MCMCglmm(Richness ~ I(Year - 2007), random = ~Site,
                         family = "poisson",  data = toolik_plants) # Doesn't converge!!
  
  
  
# Including Block and Plot as random effects (as random intecepts here)
  
  plant_mcmc <- MCMCglmm(Richness ~ I(Year-2007), random = ~Block + Plot,
                         family = "poisson", data = toolik_plants)
 
  # Creating summary of model outputs
  
  summary(plant_mcmc)
  
  
  # Checking if model has converged in MCMCglmm using trace plots 
  
  plot(plant_mcmc$VCV) # VCV to the random effects. No fuzzy caterpillar
  plot(plant_mcmc$Sol) # Sol refers to the fixed effects. Not here either. 
  # Thus model isn't the best to answer research question as we are not accounting for 
  ## site effects, or for the fact that plots are withing blocks within sites
  
  
  
  #  Estimate changes in the cover of one species - Betula nana, dwarf birch. 
  # Can use poisson distribution here 
  
  # Set weakly informative priors
  prior2 <- list(R = list(V = 1, nu = 0.002),
                 G = list(G1 = list(V = 1, nu = 1, alpha.mu = 0, alpha.v = 10000),
                          G2 = list(V = 1, nu = 1, alpha.mu = 0, alpha.v = 10000),
                          G3 = list(V = 1, nu = 1, alpha.mu = 0, alpha.v = 10000)))
  
  # Extract just the Betula nana data
  betula <- filter(toolik_plants, Species == "Bet nan")
  
  # Include all three levels of our experimental hierarchy as random intercepts
  betula_m <- MCMCglmm(round(Relative.Cover*100) ~ Year, random = ~Site + Block + Plot,
                       family = "poisson", prior = prior2, data = betula)
  
  summary(betula_m)
  plot(betula_m$VCV)
  plot(betula_m$Sol)
  
  
  # Visualizing results of Betula nana model using MCMCvis package 
  # Keeps returning Error in MCMCchains(object, params = params, excl = excl, ISB = ISB) : 
  ## Invalid object type. Input must be stanfit object (rstan), stanreg object (rstanarm), 
  ## brmsfit object (brms), mcmc.list object (coda), rjags object (R2jags), jagsUI object (jagsUI), or matrix with MCMC chains. 
 
   MCMCplot(betula_m$Sol) #
  MCMCplot(betula_m$VCV)