# Matt Prill
# prill.mattfb@gmail.com
# Fundamentals of Data Science
# Final Model Script



# Libraries----
library(tidyverse)
library(brms)
library(tidybayes)
library(beepr)

# Read Data ---- 
data <- read.csv("data/LPI_2024.csv")

# Data Manipulation ----
ox <- data %>% 
  gather("year", "pop", 31:101) %>%                  # Reshape data to long form
  filter(Common_name == "Musk ox",                   # Filter for species of interest: Musk ox 
         Region ==  "Europe",                        # Filter for the European populations (filter out Canada; not geographically or geo-politically European and only has 2 samples of very different values: would create unbalanced model)
         !is.na(as.numeric(pop))) %>%                # Remove non numeric population counts ('NA's)
  mutate(year = parse_number(year),                  # Remove characters from 'year' column to allow model to treat as integer variable.
         pop = as.integer(pop),                      # Poisson requires response variable to be integer 
         year2 = year-1950)                          # To stop repeating this function in the model 



# Bayesian Statistical Analysis ----

# Setting (uninformative) priors
priors <- c(
  set_prior("normal(0, 10)", class = "b", coef = "year2"),                 # Prior for year2
  set_prior("normal(0, 10)", class = "b", coef = "CountryNorway"),         # Prior for Country.list (Norway)
  set_prior("normal(0, 10)", class = "b", coef = "CountrySweden"),         # Prior for Country.list (Sweden)
  set_prior("normal(0, 10)", class = "Intercept"),                         # Prior for the intercept
  set_prior("exponential(1)", class = "sd")                                # Prior for random effects
)

# The model - WARNING: TAKES A FEW MINUTES TO RUN
ox_mbrms <- brms::brm(pop ~ year2 + Country + (1|year),                    # Population = response variable, year2 = explanatory variable, Country = fixed effect (3 levels), year = random effect 
                      data = ox, family = poisson(), chains = 3,           # Data shows a poisson distribution so we use this prior 
                      iter = 5000, warmup = 3000)                          # Run model 5000 times and omit the first 3000 results.
                                                                           # Would not be a crossed model as the ox will not cross between location or experience different sampling methods
                                                                           # Year is random effect rather than year2 because year2 begins at 1 to prevent the model running all years before the first sample.


summary(ox_mbrms)    # Summaries model outputs
plot(ox_mbrms)       # Plot the model
pp_check(ox_mbrms)   # Showing posterior distribution (y) and 10 random 10 random distributions created by the model (yrep) 


# Reporting the Result ----
# The model log-transformed the data because of the poisson distribution.
# Therefore...
2.22 + 0.05 # I add the intercept and year estimates
# 2.27

exp(2.27)  # and find the exponent to undo the log-transformation
# 9.67 (β = 0.06, 95% CI = 0.05 - 0.06)



# Now for population of Norwegian Muskox
exp(3.16)
# 23.6, (β = 1, 95% CI = 0.91 - 1.10)



# Therefore,
# European Muskox abundance significantly increased between 1970 and 2010 (β = 0.06, 95% CI = 0.05 to 0.06, standard error = ± 0.001).
# This means on average, the muskox population increased by 10 individuals each year: positive directional change.
# The population was also a determinant of abundance change;
# In comparison to the reference population (Greenland), Norwegian Muskox populations increased at a rate of 24 per year (β = 1.00, 95% CI = 0.91 to  1.10, standard error= ± 0.05)


# Hooray!
beep(8) # (Sound on)

