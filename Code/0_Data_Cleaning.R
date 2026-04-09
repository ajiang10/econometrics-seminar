# Libraries
library(tidygeocoder)
library(tidyverse)

# Load Data
global_power_plant_data <- read_csv("Data/global_power_plant_database.csv")

# Filter data for relevant information
global_power_plant_data_filtered <- global_power_plant_data %>%
  filter(country == "USA") %>%
  select(name, country_long, capacity_mw, primary_fuel, latitude, longitude, commissioning_year)

latlong <- global_power_plant_data_filtered %>%
  select(latitude, longitude)

reverse <- latlong %>%
  reverse_geocode(lat = latitude, long = longitude, method = 'arcgis',
                  address = address_found, full_results = TRUE)

