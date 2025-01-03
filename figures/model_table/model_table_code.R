# Matt Prill
# Model Summary Table

# Libraries----
library(tidyverse)
library(brms)
library(tidybayes)
library(sjPlot)
library(insight)
library(httr)
library(webshot)


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
         year2 = I(year-1969))                       # To stop repeating this function in the model 



# Hierarchical Bayesian Statistical Analysis----
ox_mbrms <- brms::brm(pop ~ I(year - 1969) + Country + (1|year),  
                      data = ox, family = poisson(), chains = 3, 
                      iter = 5000, warmup = 3000)  

# Generating and Saving Table----
tab_model(ox_mbrms, transform = NULL, file = "plot.html")  # Table of results
webshot("plot.html", "table.png")  # Saving as png
