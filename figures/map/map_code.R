# Matt Prill
# Mapping European Muskox popualtions
# Reference: https://ourcodingclub.github.io/tutorials/maps/

# Libraries----
library(tidyverse)
library(sf)  # Spatial data
library(rworldmap) # Maps
library(rworldxtra) # High Res maps


# Read Data ---- 
data <- read.csv("data/LPI_2024.csv")

# Data Manipulation ----
ox <- data %>% 
  gather("year", "pop", 31:101) %>%                  # Reshape data to long form
  filter(Common_name == "Musk ox",                   # Filter for species of interest: Musk ox 
         Region ==  "Europe",                        # Filter for the European populations (filter out Canada; not geographically or geo-politically European and only has 2 samples of very different values: would create unbalanced model)
         !is.na(as.numeric(pop))) %>%                # Remove non numeric population counts ('NA's)
  mutate(year = parse_number(year),                  # Remove characters from 'year' column to allow model to treat as integer variable.
         pop = as.integer(pop))                      # Poisson requires response variable to be integer 

colnames(ox) # long and latitude coords


# Quick plot
ggplot(data = ox, aes(x = Longitude, y = Latitude, colour = Country)) +
  geom_point()


world <- getMap(resolution = "high") 

# Whole World
ggplot() +
    geom_polygon(data = world, 
                 aes(x = long, y = lat, group = group),
                 fill = NA, colour = "black") + 
    geom_point(data = ox,  
               aes(x = Longitude, y = Latitude, 
                   colour = Country)) +
    coord_quickmap() +  
    theme_classic() +  
    xlab("Longitude") +
    ylab("Latitude") + 
    guides(colour=guide_legend(title="Species"))
  


saf_countries <- c("Greenland", "Norway", "Sweden") 


world_saf <- world[world@data$ADMIN %in% saf_countries, ] 


ggplot() +
    geom_polygon(data = world_saf, 
                 aes(x = long, y = lat, group = group),
                 fill = "beige", colour = "black")  + 
    geom_point(data = ox,  # Add and plot speices data
               aes(x = Longitude, y = Latitude, 
                   colour = Country), size = 8) +
    coord_quickmap() + 
    xlim(-74, 40) + 
    ylim(55, 85) +
  theme(
    panel.background = element_rect(fill = "lightblue", color = NA),
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 3),
    legend.title = element_text(size = 14, face = "bold"), 
    legend.text = element_text(size = 12), 
    legend.key.size = unit(1, "cm"),
    legend.key = element_blank(),
    axis.text = element_text(size = 15),
    axis.title = element_text(size =20))  + 
    xlab("\nLongitude") +
    ylab("Latitude\n") +
  scale_colour_manual(
    values = c("springgreen4", "red4", "lightgoldenrod3"),
    guide = guide_legend(override.aes = list(size = 15),
                         title = "Muskox Population Location",
                         title.theme = element_text(size = 18, face = "bold"),
                         label.theme = element_text(size = 20)))

