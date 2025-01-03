# Matt Prill
# Model Credibility Interval Plot

# Libraries----
library(tidyverse)
library(brms)
library(tidybayes)


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
ox_mbrms <- brms::brm(pop ~ year2 + Country + (1|year),  
                      data = ox, family = poisson(), chains = 3, 
                      iter = 3000, warmup = 1000) 


# Plots ----
# With separate lines for location

(model_fit_ox <- ox %>%
   add_predicted_draws(ox_mbrms) %>%  # Adding the posterior distribution
   ggplot(aes(x = year, y = pop, color = ordered(Country),  # Key for the credibility intervals and geom point is combined
              fill = ordered(Country)))) +
  stat_lineribbon(aes(y = .prediction), .width = c(0.95, 0.80, 0.50), alpha = 0.25) +  # Regression lines and credibility intervals (.predict takes the values from the model)
  geom_point(data = ox) +  # Add raw data
  scale_fill_manual(values = c("springgreen4", "red4", "lightgoldenrod3")) +  # Colours
  scale_color_manual(values = c("springgreen4", "red4", "lightgoldenrod3")) +  # Colours
  ylab("European Muskox Abundance\n") +  # Y-axis label
  xlab("\nYear") +  # X-axis label
  theme_bw() +  # Clearer background
  theme(legend.title = element_blank(),  # Theme layout (legend, text etc.)
        legend.text = element_text(size = 14),
        legend.key.size = unit(1.5, "cm"), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, vjust = -15, size = 16), 
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 2))
