# Matt Prill
# European Muskox Population Histogram

# Libraries----
library(tidyverse)

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



# Histogram Showing Raw Collective Distribution
(ox_distribution<- ggplot(ox, aes(x = pop)) +
   geom_histogram(colour = "black", fill = "lightsalmon3", bins =  36) +  # Histogram
   scale_x_continuous(expand = c(0,0), limits = c(3.2,240)) +  # X-axis formatting
   scale_y_continuous(expand = c(0,0), limits = c(0,16)) +  # Y-axis formatting 
   ggtitle("European Muskox Abundance Histogram (1970 - 2010)") +  # Title
   ylab("Frequency\n") +  # Y-axis title
   xlab("\nMuskox Abundance") +  # X-axis title
   theme(axis.text = element_text(size = 12),  # Axis formatting
         axis.title = element_text(size = 30, face = "plain")) +
   theme_classic()) +  # Theme (clear background)
   theme(plot.title = element_text(hjust = 0.5, vjust = -15, size = 16), 
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 20), 
        panel.border = element_rect(color = "black",
                                    fill = NA,
                                    size = 2))
