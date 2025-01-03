# Matt Prill
# Box and Dot Plot for Muskox Abundance by Country

# Libraries----
library(tidyverse)
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



# Box and dot plot showing need for Country as fixed effect ----
ox %>%
  ggplot(aes(x = Country,
             y = pop,
             fill = Country)) +
  stat_boxplot(geom = "errorbar", width = 0.2, size = 0.5, coef = Inf) +  # Whiskers
  geom_boxplot(width = 0.5, outlier.shape = NA) +  # Do not ignore outliers
  geom_violin(alpha = 0.3, trim = T, size = 0.9, scale = 'width', colour = "black") + # Violin
  geom_jitter(color = "black", width = 0.12, height= 0, size = 1.2, alpha = 0.9) +
  ylab(bquote("Muskox Abundance\n")) +
  xlab("\nCountry") +
  ggtitle("European Muskox Population Abundances (1970 - 2010)") +  # Title
  theme_classic() + # theme_bw() to add 
  scale_y_continuous(expand = c(0,0), limits = c(0,250)) +
  scale_fill_manual(values = c("springgreen4", "red4", "lightgoldenrod3")) + # Choosing colours
  theme(axis.text=element_text(size = 16),
        axis.title=element_text(size = 16),
        legend.position = "none", 
        plot.title = element_text(hjust = 0.5, vjust = -15),  # Title formatting
        panel.border = element_rect(color = "black",
                                    fill = NA, size = 2))
