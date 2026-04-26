library(tidyverse)
library(ggplot2)
library(sf)
library(tigris)
library(extrafont)
loadfonts(device = "postscript") # Load fonts for plotting

# Load cleaned data
county_panel <- readRDS("Data/cleaned_county_panel.rds")
power_plants <- readRDS("Data/cleaned_power_plant_data.rds") %>%
  filter(commissioning_year >= 2002) # Relevant years for our analysis

# Summary statistics for county GDP data
gdp_year_summary <- county_panel %>%
  group_by(Year) %>%
  summarise(
    Mean_GDP = mean(Real_GDP, na.rm = TRUE),
    Median_GDP = median(Real_GDP, na.rm = TRUE),
    SD_GDP = sd(Real_GDP, na.rm = TRUE),
    Min_GDP = min(Real_GDP, na.rm = TRUE),
    Max_GDP = max(Real_GDP, na.rm = TRUE)
  ) 
print(gdp_year_summary)

gdp_county_summary <- county_gdp %>%
  group_by(GeoName) %>%
  summarise(
    Mean_GDP = mean(Real_GDP, na.rm = TRUE),
    Median_GDP = median(Real_GDP, na.rm = TRUE),
    SD_GDP = sd(Real_GDP, na.rm = TRUE),
    Min_GDP = min(Real_GDP, na.rm = TRUE),
    Max_GDP = max(Real_GDP, na.rm = TRUE)
  )
print(gdp_county_summary)

plot_gdp_year <- gdp_year_summary %>%
  ggplot(aes(x = Year, y = Mean_GDP)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(
    x = "Year",
    y = "Mean Real GDP",
    title = "Mean Real GDP by Year"
  ) +
  theme_minimal()

plot_gdp_year

# Summary statistics for power plant data
power_plant_summary <- power_plants %>%
  group_by(commissioning_year) %>%
  summarise(
    Total_Plants = n(),
    Total_Capacity_MW = sum(capacity_mw, na.rm = TRUE)
  )
print(power_plant_summary)

power_plant_fuel_summary <- power_plants %>%
  mutate(commissioning_year = floor(commissioning_year)) %>%
  group_by(commissioning_year, primary_fuel) %>%
  summarise(
    Total_Plants = n(),
    Total_Capacity_MW = sum(capacity_mw, na.rm = TRUE)
  ) %>%
  filter(primary_fuel %in% c("Wind", "Gas", "Solar", "Coal", "Hydro", "Nuclear"))

plot1 <- power_plant_summary %>%
  ggplot(aes(x = commissioning_year, y = Total_Plants, group = 1)) +
  geom_line(color = "black") +
  geom_point(color = "black") +
  labs(
    x = "Commissioning Year",
    y = "Total Power Plants",
    title = "Total Power Plants by Commissioning Year"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Linux Libertine"))

plot1

plot2 <- power_plant_fuel_summary %>%
  ggplot(aes(x = commissioning_year, y = Total_Plants, color = primary_fuel, group = primary_fuel)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Commissioning Year",
    y = "Total Power Plants",
    title = "Total Power Plants by Primary Fuel",
    color = "Primary Fuel Type"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Linux Libertine"))

plot2

plot3 <- power_plant_summary %>%
  ggplot(aes(x = commissioning_year, y = Total_Capacity_MW, group = 1)) +
  geom_line(color = "black") +
  geom_point(color = "black") +
  labs(
    x = "Commissioning Year",
    y = "Total Capacity (MW)",
    title = "Total Power Plant Capacity by Commissioning Year"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Linux Libertine"))

plot3

plot4 <- power_plant_fuel_summary %>%
  ggplot(aes(x = commissioning_year, y = Total_Capacity_MW, color = primary_fuel, group = primary_fuel)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Commissioning Year",
    y = "Total Capacity (MW)",
    title = "New Power Plant Capacity by Primary Fuel",
    color = "Primary Fuel Type"
  ) +
  theme_minimal() +
  theme(text = element_text(family = "Linux Libertine"))

plot4

us_states <- states(cb = TRUE) %>%
  filter(!STUSPS %in% c("AK", "HI", "AS", "GU", "PR", "MP", "VI")) # Exclude non-continental states and territories

plant_map <- ggplot() +
     geom_sf(data = us_states, fill = NA, color = "gray45") + # borders of states
     geom_sf(data = power_plants %>%
  filter(primary_fuel %in% c("Wind", "Gas", "Solar", "Coal", "Hydro", "Nuclear")), pch = 1, aes(color = primary_fuel)) + # the power plants
     theme_void() +
     labs(title = "US Power Plants 2002-2020") +
     theme(plot.title = element_text(hjust = 1/2, family = "Linux Libertine"), legend.text = element_text(family = "Linux Libertine"))

plant_map
