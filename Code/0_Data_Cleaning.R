# Libraries
library(tidygeocoder)
library(tidyverse)

# Load Data
global_power_plant_data <- read_csv("Data/global_power_plant_database.csv")

# Load the geofips linking data
geofips <- read_delim("Data/county_adjacency2025.txt", delim = "|")

# Turn County Name into County and State
geofips_linking <- geofips %>%
  select("County Name", "County GEOID") %>%
  distinct() %>%
  separate(`County Name`, into = c("County", "State"), sep = ", ") %>%
  filter(!State %in% c("AS", "PR", "GU", "VI", "MP")) %>%
  mutate(State_Full_Name = state.name[match(State, state.abb)]) %>%
  mutate(State_Full_Name = ifelse(State == "DC", "District of Columbia", State_Full_Name))

# Function to convert "City of X" to "X City"
convert_city_of <- function(x) {
  x |> 
    str_squish() |> 
    str_replace(regex("^city of\\s+(.+)$", ignore_case = TRUE), "\\1 city")
}

# Filter data for relevant information
global_power_plant_data_filtered <- global_power_plant_data %>%
  filter(country == "USA") %>%
  select(name, country_long, capacity_mw, primary_fuel, latitude, longitude, commissioning_year)

latlong <- global_power_plant_data_filtered %>%
  select(latitude, longitude)

reverse <- latlong %>%
  reverse_geocode(lat = latitude, 
                  long = longitude, method = 'arcgis',
                  address = address_found, 
                  full_results = TRUE) #WARNING: This step may take a while due to the number of rows and the nature of reverse geocoding.

reverse <- reverse %>%
  select(latitude, longitude, address_found, City, Subregion, Region)

# Combine the reverse geocoded data with the original filtered data and perform necessary cleaning and transformationsss
final_power_plant_data <- global_power_plant_data_filtered %>%
  full_join(reverse, by = c("latitude", "longitude")) %>%
  filter(!Region %in% c("Puerto Rico", "Guam", "Coahuila de Zaragoza")) %>%
  mutate(Region = ifelse(Region == "District of Columbia", "DC", Region)) %>%
  mutate(Subregion = ifelse(Subregion == "Washington County" & City == "Washington" & Region == "DC", "District of Columbia", Subregion)) %>%
  mutate(Region = ifelse(Region == "DC", "District of Columbia", Region)) %>%
  mutate(Subregion = ifelse(Subregion == "DeSoto Parish", "De Soto Parish", Subregion)) %>%
  mutate(Subregion = convert_city_of(Subregion)) %>%
  mutate(Subregion = ifelse(Subregion == "City and County of Honolulu", "Honolulu County", Subregion)) %>%
  mutate(Subregion = ifelse(Subregion == "City and County of Denver", "Denver County", Subregion)) %>%
  mutate(Subregion = ifelse(Subregion == "Municipality of Anchorage", "Anchorage Municipality", Subregion)) %>%
  mutate(Subregion = ifelse(Subregion == "Municipality of Skagway", "Skagway Municipality", Subregion)) %>%
  mutate(Subregion = ifelse(Subregion == "City and Borough of Juneau", "Juneau City and Borough", Subregion)) %>%
  mutate(Subregion = ifelse(Subregion == "City and Borough of Sitka", "Sitka City and Borough", Subregion)) %>%
  mutate(Subregion = ifelse(Subregion == "City and Borough of Wrangell", "Wrangell City and Borough", Subregion)) %>%
  mutate(Subregion = ifelse(City == "Town of Bridgeport", "Greater Bridgeport Planning Region", Subregion)) %>%
  mutate(Subregion = ifelse(City == "Town of Danbury", "Western Connecticut Planning Region", Subregion)) %>%
  left_join(geofips_linking, by = c("Subregion" = "County", "Region" = "State_Full_Name")) 

# Save the cleaned data to a new CSV file
write_csv(final_power_plant_data, "Data/cleaned_power_plant_data.csv")

# Load the full county GDP data
full_county_gdp_data <- read_csv("Data/county_real_gdp_2001_2024.csv")

# Clean and reshape the full county GDP data
full_county_gdp_filtered <- full_county_gdp_data %>%
  filter(LineCode == 1) %>% # Filter for real GDP
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Real_GDP") %>%
  select(GeoFIPS, GeoName, Year, Real_GDP)

full_county_linked <- full_county_gdp_filtered %>%
  left_join(geofips_linking, by = c("GeoFIPS" = "County GEOID")) %>%
  select(GeoFIPS, GeoName, Year, Real_GDP, County, State) %>%
  drop_na(County, State) %>%
  mutate(State_Full_Name = state.name[match(State, state.abb)]) %>%
  mutate(State_Full_Name = ifelse(State == "DC", "District of Columbia", State_Full_Name))

# Add GDP Growth
full_county_growth <- full_county_linked %>%
  mutate(Real_GDP = as.numeric(Real_GDP)) %>%
  group_by(GeoFIPS) %>%
  arrange(Year) %>%
  mutate(GDP_Growth = ((Real_GDP - lag(Real_GDP)) / lag(Real_GDP)) * 100) %>%
  ungroup()

# Save the cleaned and linked county GDP data to a new CSV file
write_csv(full_county_growth, "Data/cleaned_county_gdp_data.csv")
