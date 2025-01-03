# Matt Prill
# Population trends by Country 

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


# Function for Plot : Makes it cleaner
plot_theme <- function(...){  # function to make the following pop distribution graphs
  theme_bw() +
    theme(
      # adjust axes
      axis.line = element_blank(),
      axis.text = element_text(size = 16,
                               color = "black"),
      axis.text.x = element_text(margin = margin(5, b = 10)),
      axis.title = element_text(size = 16,
                                color = 'black'),
      axis.ticks = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
      plot.background = element_rect(fill = "white",
                                     color = NA),
      panel.background = element_rect(fill = "white",
                                      color = NA),
      legend.background = element_rect(fill = NA,
                                       color = NA),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 16, hjust = 0,
                                 color = "black"),
      plot.title = element_text(size = 20,
                                color = 'black',
                                margin = margin(10, 10, 10, 10),
                                hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = "black",
                                   margin = margin(0, 0, 30, 0))
    )
  
}




# Distributions of abundance for the 3 separate populations
(all_distributions <- ox %>%                            
    ggplot(aes(x = pop)) +
    geom_density() +  #  geom_density() instead of geom_histogram
    facet_wrap(~ Country, scale = 'free') +  # create the grid based on the id, scale = 'free' allows different x and y scale for each population
    labs(y = 'Frequency\n',
         x = '\nAbundance\n',
         title = 'Distributions of European Muskox populations',
         caption = 'Data Source: Living Planet Index') +
    plot_theme() +  # function defined above
    theme(axis.text.x = element_blank(),  # remove the axis labels
          axis.text.y = element_blank(),
          strip.text = element_text(color = "white"),
          strip.background = element_rect(fill = "black", colour = "black")))

