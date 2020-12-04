# Introduction----
# Intro to stan: Getting started with Bayesian Modelling in Stan 
#Nicole Yap s1761850@ed.ac.uk

# Libraries----

library(rstan)
library(gdata)
library(bayesplot)

# Load data----

# Adding stringsAsFactors = F means that numeric variables won't be
# read in as factors/categorical variables
seaice <- read.csv("Data/seaice.csv", stringsAsFactors = F)


# Looking at the data
head(seaice)


# Change column names 
colnames(seaice) <- c("year", "extent_north", "extent_south")


# Making figure showing change in sea ice extent in the N. Hemi over time

plot(extent_north ~ year, pch = 20, data = seaice)


# Running general linear model using lm() 

lm1 <- lm(extent_north ~ year, data = seaice)
summary(lm1)


# Adding model fit to plot

abline(lm1, col = 2, lty = 2, lw = 3)


# Data Preperation----

# Let’s rename the variables and index the years from 1 to 39. 
# Set up our year data to index from 1 to 30 years.

x <- I(seaice$year - 1978)
y <- seaice$extent_north
N <- length(seaice$year)


# Re-run linear model with prepared data

lm1 <- lm(y ~ x)
summary(lm1)


# Extracting key summary stats from simple model to compare with Stan model ouputs later 

lm_alpha <- summary(lm1)$coeff[1]  # the intercept
lm_beta <- summary(lm1)$coeff[2]  # the slope
lm_sigma <- sigma(lm1)  # the residual error


# Turning simple model into dataframe for inputting into Stan model 

stan_data <- list(N = N, x = x, y = y)


# Writing linear model in language Stan---- 

write("// Stan model for simple linear regression

data {
 int < lower = 1 > N; // Sample size
 vector[N] x; // Predictor
 vector[N] y; // Outcome
}

parameters {
 real alpha; // Intercept
 real beta; // Slope (regression coefficients)
 real < lower = 0 > sigma; // Error SD
}

model {
 y ~ normal(alpha + x * beta , sigma);
}

generated quantities {
} // The posterior predictive distribution",

"Output/stan_model1.stan")

# Checking Stan model to make sure we wrote a file
stanc("Output/stan_model1.stan")

# Saving file path
stan_model1 <- "Output/stan_model1.stan"

# Fitting out model using the stan() function----

fit <- stan(file = stan_model1, data = stan_data, warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1)


# Getting summary stats for  parameter estimates, and sampler diagnostics by executing the name of the object

fit # Rhat value = 1, so chains have converged!


# Viewing posterior of our parameters by extracting them from the model object 

posterior <- extract(fit) # Extract() puts the posterior estimates for each parameter into a list
str(posterior)


# Comparing previous estimate with lm()
# hint: They're the same

plot(y ~ x, pch = 20)

abline(lm1, col = 2, lty = 2, lw = 3)
abline( mean(posterior$alpha), mean(posterior$beta), col = 6, lw = 2) # Result is identical to lm() output


# Visualize the variability in estimation of the regression plot by plotting multiple estimates from posterior

for (i in 1:500) {
        abline(posterior$alpha[i], posterior$beta[i], col = "gray", lty = 1)
}

plot(y ~ x, pch = 20)

for (i in 1:500) {
        abline(posterior$alpha[i], posterior$beta[i], col = "gray", lty = 1)
}

abline(mean(posterior$alpha), mean(posterior$beta), col = 6, lw = 2) # Adding stan linear model fit


# Changing priors---- 

# Using more informative priors- normal priors with small standard deviations, 
## for the relationshio between sea ice and time 


write("// Stan model for simple linear regression

data {
 int < lower = 1 > N; // Sample size
 vector[N] x; // Predictor
 vector[N] y; // Outcome
}

parameters {
 real alpha; // Intercept
 real beta; // Slope (regression coefficients)
 real < lower = 0 > sigma; // Error SD
}

model {
 alpha ~ normal(10, 0.1);
 beta ~ normal(1, 0.1);
 y ~ normal(alpha + x * beta , sigma);
}

generated quantities {}",

"Script/stan_model2.stan")

stan_model2 <- "Script/stan_model2.stan"


# Fit this new model with informative priors and compare it to the mean estimate using the uniform priors

fit2 <- stan(stan_model2, data = stan_data, warmup = 500, iter = 1000, chains = 4, cores = 2, thin = 1)

posterior2 <- extract(fit2)

plot(y ~ x, pch = 20)

abline(alpha, beta, col = 4, lty = 2, lw = 2)
abline(mean(posterior2$alpha), mean(posterior2$beta), col = 3, lw = 2)
abline(mean(posterior$alpha), mean(posterior$beta), col = 36, lw = 3)


# Checking effective sample size (n_eff), Rhat values and traceplots of model parameters

fit # Rhat is near or at 1 so model has converged, n_eff is over 100 is is fine 


# Viewing traceplots directly from the posterior

plot(posterior$alpha, type = "l")
plot(posterior$beta, type = "l")
plot(posterior$sigma, type = "l")


# Trying to run model for only 50 iterations and checking traceplots 
# hint* "divergent transitions" indicate mis-specified model, or that sampler has failed to 
## fully sample the posterior or both. "Divergent transitions" indicate roblems with model!

fit_bad <- stan(stan_model1, data = stan_data, warmup = 25, iter = 50, chains = 4, cores = 2, thin = 1)
posterior_bad <- extract(fit_bad)

plot(posterior_bad$alpha, type = "l")
plot(posterior_bad$beta, type = "l")
plot(posterior_bad$sigma, type = "l")


# Obtaining summaries of parameters through posterior directly and plotting non-Bayesian linear model values----
# Getting density plot distributions from the Stan model fit compared with estimates from the general lm() fit

par(mfrow = c(1,3))

plot(density(posterior$alpha), main = "Alpha")
abline(v = lm_alpha, col = 4, lty = 2)

plot(density(posterior$beta), main = "Beta")
abline(v = lm_beta, col = 4, lty = 2)

plot(density(posterior$sigma), main = "Sigma")
abline(v = lm_sigma, col = 4, lty = 2)


# Calculating the probability of any parameter being over/under certain value 

# Probability that beta > 0 

sum(posterior$beta>0)/length(posterior$beta)
# 0


# Probability that beta > 0.2 

sum(posterior$beta>0.2)/length(posterior$beta)
# 0


# Diagnostic plots in rstan

# Geting trace plots of different chains of the Stan model 

traceplot(fit)


# Looking at posterior densities and histograms for the intercept, slope and residual variance 
## from Stan model

stan_dens(fit)
stan_hist(fit)


# Generate plots which indicate the mean parameter estimates and any credible intervals from Stan model 

plot(fit, show_density = FALSE, ci_level = 0.5, outer_level = 0.95, fill_color = "salmon")


# Posterior predictive checks----

# Generate predictions that also represent uncertainties in the model and data 
## generation process using Generated Quantities block. 

write("// Stan model for simple linear regression

data {
 int < lower = 1 > N; // Sample size
 vector[N] x; // Predictor
 vector[N] y; // Outcome
}

parameters {
 real alpha; // Intercept
 real beta; // Slope (regression coefficients)
 real < lower = 0 > sigma; // Error SD
}

model {
 y ~ normal(x * beta + alpha, sigma);
}

generated quantities {
 real y_rep[N];

 for (n in 1:N) {
 y_rep[n] = normal_rng(x[n] * beta + alpha, sigma);
 }

}",

"Script/stan_model2_GQ.stan")

stan_model2_GQ <- "Script/stan_model2_GQ.stan"


# Vectorization via putting it in a loop 

fit3 <- stan(stan_model2_GQ, data = stan_data, iter = 1000, chains = 4, cores = 2, thin = 1)


# Extracting y_rep values from posterior---- 

y_rep <- as.matrix(fit3, pars = "y_rep")
dim(y_rep)


# Comparing density of y with densities of y over 200 random posterior draws

ppc_dens_overlay(y, y_rep[1:200, ]) # Estimates across random posterior draws


# Using above to compare estimates of summary stats

ppc_stat(y = y, yrep = y_rep, stat = "mean")


# Investigate mean posterior prediction per datapoint vs the observed value for each datapoint
## (default line is 1:1)

ppc_scatter_avg(y = y, yrep = y_rep)


# Viewing current available bayesplot options

available_ppc()


# View colour scheme in bayesplot

color_scheme_view(c("blue", "gray", "green", "pink", "purple",
                    "red","teal","yellow"))


# Mix colour scheme

color_scheme_view("mix-blue-red")


# Set colour schemes using colour_scheme_set()

color_scheme_set("blue")
