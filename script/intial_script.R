# Matt Prill
# Fundamentals of Data Science

# Starter code ----

# Libraries
library(tidyverse)  # Range of packages e.g. dplyr
library(lme4)  # Linear Mixed modelling
library(brms)  # Bayesian Modelling
library(tidybayes)  # Add predicted draws

# Read Living Planet Data
data <- read.csv("data/LPI_2024.csv")



# Filtering for Muskox
ox <- data %>% 
  gather("year", "pop", 31:101) %>%  # Reshape data to longform
  filter(Common_name == "Musk ox",  # Filter Muskox
         Region ==  "Europe",  # Filter for the European populations (filter out canada as it only has 2 samples of very different values: would creaet unbalanced model
         !is.na(as.numeric(pop))) %>%  # Remove non numeric population counts
  mutate(year = parse_number(year)) 


# Statistical analysis ----

# linear mixed with country and sampling Units as fixed effect (confounding factor)
# There are not enough levels to these variables to include as random factors
mixed.lm <- lm(pop ~ year + Country + Units, data = ox)
summary(mixed.lm) # Potentially overfitted script



# Base ----
ox$pop <- as.integer(ox$pop)  # Model wont accept 'numeric' response variable because poisson is integer

ox_mbrms <- brms::brm(pop ~ I(year - 1969),  # Response Variable = pop. Year to start at 1 (I indicates integer)
                           data = ox, family = poisson(), chains = 3,  # Incorporating poisson distribution
                           iter = 3000, warmup = 1000)  # iter is the number of model runs (3000)

summary(ox_mbrms)  # Strictly positive credibility intervals = year has significant positive effect on population

# Complex model ----
# Adding random effects----
unique(ox$Country)  # observations come from 3 locations

ox2_mbrms <- brms::brm(pop ~ I(year - 1969) + (1|year),  # Response Variable = pop. Year to start at 1 (I indicates integer)
                      data = ox, family = poisson(), chains = 3,  # Incorporating poisson distribution
                      iter = 3000, warmup = 1000)  # iter is the number of model runs (3000

summary(ox2_mbrms)

# Location as fixed effect ----
ox3_mbrms <- brms::brm(pop ~ I(year - 1969) + Country,
                           data = ox, family = poisson(), chains = 3,  # Incorporates evidence beliefs or prior beliefs. Is my prior strong or weak (does it lean towards prior (thats a strong prior)) We usually use weak priors in ecology.
                           iter = 3000, warmup = 1000)
summary(ox3_mbrms)
plot(ox3_mbrms)
pp_check(ox3_mbrms) 

# Estimate for Norway is significantly more positive than other locations

# Adding Location and Sampling method as fixed effects and year as random (Too complex of a model - does not converge) ----
ox4_mbrms <- brms::brm(pop ~ I(year - 1969) + Country.list + Sampling.Method + (1|year),  # Did not converge
                       data = ox, family = poisson(), chains = 3,
                       iter = 3000, warmup = 1000)

summary(ox4_mbrms) # Sample size becomes too small an all effects become insignificant 
plot(ox4_mbrms)
pp_check(ox4_mbrms)

# Removing sampling method from model
ox5_mbrms <- brms::brm(pop ~ I(year - 1969) + Country  + (1|year),  # Did not converge
                       data = ox, family = poisson(), chains = 3,
                       iter = 3000, warmup = 1000)

summary(ox5_mbrms)  
plot(ox5_mbrms)
pp_check(ox5_mbrms)


 





# Model and data visualisation ----

ox %>% 
  ggplot(aes(x = year, y = pop)) +
  geom_point() +
  scale_y_continuous(trans='log2') +
  theme_classic() +
  geom_smooth(method='lm')


# Histogram to show distribution of data
# Shows poisson distribution
(muskox_dist <- ggplot(ox, aes(x = pop)) +
    geom_histogram(colour = "#CD8", fill = "#8D4") +
    theme_bw() +
    ylab("Count\n") +
    xlab("\nMusox abundance") +  #
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain")))

# Location boxplot shows need for Country as fixed effect (confounding factor) 
# Fixed rather than random as only 3 levels 
(boxplot_location <- ggplot(ox, aes(Country, pop)) +
    geom_boxplot() +  # could be a significant effect between locations so should look at that
    theme_bw() +
    xlab("Location\n") +
    ylab("\nMuskox population") +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 14, face = "plain"))) 

  
# Bayesian plot----
plot(ox_mbrms)

# posterior predictive checks
pp_check(ox_mbrms)
# yrep = 10 random distributions of the model


# Complex plot----
plot(ox2_mbrms)

pp_check(ox_mbrms)

# ox 3 plot
plot(ox3_mbrms)
pp_check(ox3_mbrms)



# Plotting the chosen model ----

# Without separate lines for location
(model_fit_ox <- ox %>%
    add_predicted_draws(ox3_mbrms) %>%  # adding the posterior distribution
    ggplot(aes(x = year, y = pop)) +  
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50),  # regression line and credibility interval
                    alpha = 0.5, colour = "black") +
    geom_point(data = ox, colour = "#8D4", size = 3) +   # adding raw data
    scale_fill_brewer(palette = "Greys") +
    ylab("Muskox abundance\n") +  
    xlab("\nYear") +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = c(0.15, 0.85)))

# With seperate lines for location
(location_fit_ox <- ox %>%
    group_by(Country) %>%
    add_predicted_draws(ox3_mbrms) %>%
    ggplot(aes(x = year, y = pop, color = ordered(Country), fill = ordered(Country))) +
    stat_lineribbon(aes(y = .prediction), .width = c(.95, .80, .50), alpha = 1/4) +
    geom_point(data = ox) +
    scale_fill_brewer(palette = "BuPu") +
    scale_color_brewer(palette = "Paired") +
    theme_bw() +
    ylab("European Muskox abundance\n") +
    xlab("\nYear") +
    theme_bw() +
    theme(legend.title = element_blank()))

# I need to here because the model log-transformed the data because of the poisson distribution.
2.22 + 0.05 
# = 2.27
# Exponential of that value to undo the log-transformation
exp(2.27)
# 9.689
# This means on average, the muskox population increased by 10 individuals each year: positive directional change.
