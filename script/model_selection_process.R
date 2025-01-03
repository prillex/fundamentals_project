# Matt Prill
# Fundamentals of Data Science
# Unsuccessful Models

# Libraries----
library(tidyverse)  # Range of packages e.g. dplyr
library(lme4)  # Linear Mixed modelling
library(brms)  # Bayesian Modelling
library(tidybayes)  # Add predicted draws
# ----
# ----
# Read Data---- 
data <- read.csv("data/LPI_2024.csv")

# ----
# ----
# Data Wrangling ----
ox <- data %>% 
  gather("year", "pop", 31:101) %>%  # Reshape data to long form
  filter(Common_name == "Musk ox",  # Filter for species of interest: Musk ox 
         Region ==  "Europe",  # Filter for the European populations (filter out Canada; not geographically or geo-politically European and only has 2 samples of very different values: would create unbalanced model)
         !is.na(as.numeric(pop))) %>%  # Remove non numeric population counts ('NA's)
  mutate(year = parse_number(year),  # Remove characters from 'year' column to allow model to treat as integer variable.
         pop = as.integer(pop))  # Poisson requires response variable to be integer


# ----
# ----
# Models ----

# Linear Model ----

# Example of mixed linear model (no assumption checks - I will be doing this for Bayesian)
mixed.lm <- lm(pop ~ year + Country + Units, data = ox)
summary(mixed.lm) # Potentially overfitted script, 

# ----
# ----
# Bayesian Hierarchical Linear Models ----

# Model for Country as Fixed Effect
ox_mbrms_country <- brms::brm(pop ~ I(year - 1950) + Country,  # Population = response variable, year = explanatory variable, Country = fixed effect (3 levels)
                           data = ox, family = poisson(), chains = 3,  # Data shows a poisson distribution so we use this prior 
                           iter = 5000, warmup = 3000)
# Summary Table
summary(ox_mbrms_country)

# Plot for Country as Fixed Effect 
(location_fit_ox <- ox %>%
    group_by(Country) %>%  # Group by Country so: Key = Country
    add_predicted_draws(ox_mbrms_country) %>%  # Adding the posterior distribution
    ggplot(aes(x = year, y = pop, color = ordered(Country),  # Splitting distributions between Country 
               fill = ordered(Country)))) +
  stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50), alpha = 1/4) +  # Regression lines and Credibility Intervals 
  geom_point(data = ox) +  # Add raw data
  scale_fill_manual(values = c("springgreen4", "red4", "lightgoldenrod3")) +  # Colours
  scale_color_manual(values = c("springgreen4", "red4", "lightgoldenrod3")) +  # Colours
  theme_bw() +
  ylab("European Muskox abundance\n") +  # Y-axis label
  xlab("\nYear") +  # X-axis label
  theme_bw() +
  theme(legend.title = element_blank())


# Model for Year as Random Effect----
ox_mbrms_year <- brms::brm(pop ~ I(year - 1950) + (1|year),  # Population = response variable, year = explanatory variable, Country = fixed effect (3 levels), Year = Random Effect
                      data = ox, family = poisson(), chains = 3,  # Data shows a poisson distribution so we use this prior 
                      iter = 5000, warmup = 3000)

# Summary Table
summary(ox_mbrms_year)

(year_fit_ox <- ox %>%
    group_by(year) %>%  # Group by Country so: Key = Country
    add_predicted_draws(ox_mbrms_year) %>%  # Adding the posterior distribution
    ggplot(aes(x = year, y = pop, color = ordered(Country.list),  # Splitting distributions between Country 
               fill = ordered(Country.list)))) +
  stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50), alpha = 1/4) +  # Regression lines and Credibility Intervals 
  geom_point(data = ox) +  # Add raw data
  scale_fill_manual(values = c("springgreen4", "red4", "lightgoldenrod3")) +  # Colours
  scale_color_manual(values = c("springgreen4", "red4", "lightgoldenrod3")) +  # Colours
  theme_bw() +
  ylab("European Muskox abundance\n") +  # Y-axis label
  xlab("\nYear") +  # X-axis label
  theme_bw() +
  theme(legend.title = element_blank())


# Model Incorporating Country, Year and Sampling Method (WARNING - TAKES TIME TO RUN AND DOES NOT CONVERGE)
ox_mbrms_sampling <- brms::brm(pop ~ I(year - 1950) + (1|year) + Country + Units,  # 2 previous models combined + Sampling.Method
                           data = ox, family = poisson(), chains = 3,  # Data shows a poisson distribution 
                           iter = 5000, warmup = 3000)
# Summary Table
summary(ox_mbrms_sampling) # This model does not converge (Rhat ≠ 1)

# This prompted the removal of 'Sampling Method' as an effect.
# I opted to remove it rather than year as the sampling method was already unique to country which is already accounted for




# Final model and LOO validation - ASSESSING + COMPARING THE FIT OF THE MODELS ----
# Hierarchical Bayesian Statistical Analysis----
ox_mbrms <- brms::brm(pop ~ year2 + Country + (1|year),  # Population = response variable, year2 = explanatory variable, Country = fixed effect (3 levels), year = random effect 
                      data = ox, family = poisson(), chains = 3,  # Data shows a poisson distribution so we use this prior 
                      iter = 5000, warmup = 3000)  # Run model 5000 times and omit the first 3000 results.
# Would not be a crossed model as the ox will not cross between location or experience different sampling methods
# Year is random effect rather than year2 because year2 begins at 1 to prevent the model running all years before the first sample.

summary(ox_mbrms)  # Summaries model outputs
plot(ox_mbrms)  # Plot the model
pp_check(ox_mbrms) # Showing posterior distribution (y) and 10 random 10 random distributions created by the model (yrep) 


(location_fit_ox <- ox %>%
    group_by(Country) %>%  # Group by Country so: Key = Country
    add_predicted_draws(ox_mbrms) %>%  # Adding the posterior distribution
    ggplot(aes(x = year, y = pop, color = ordered(Country),  # Splitting distributions between Country 
               fill = ordered(Country)))) +
  stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50), alpha = 1/4) +  # Regression lines and Credibility Intervals 
  geom_point(data = ox) +  # Add raw data
  scale_fill_manual(values = c("springgreen4", "red4", "lightgoldenrod3")) +  # Colours
  scale_color_manual(values = c("springgreen4", "red4", "lightgoldenrod3")) +  # Colours
  theme_bw() +
  ylab("European Muskox abundance\n") +  # Y-axis label
  xlab("\nYear") +  # X-axis label
  theme_bw() +
  theme(legend.title = element_blank())



# Based on elpd (higher value = better fit)
loo(ox_mbrms_country, ox_mbrms_year, ox_mbrms_sampling, ox_mbrms, compare = TRUE)  # Second model is best fit (higher elpd_diff and pp fit better, no divergent transitions)

