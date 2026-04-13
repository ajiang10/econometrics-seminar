library(tidyverse)
library(ggplot2)
county_gdp <- read_csv("Data/cleaned_county_gdp_data.csv")
power_plants <- read_csv("Data/cleaned_power_plant_data.csv") %>%
  filter(commissioning_year >= 2000)

# Summary statistics for county GDP data
gdp_year_summary <- county_gdp %>%
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

gdp_year_growth_summary <- county_gdp %>%
  group_by(Year) %>%
  summarise(
    Mean_GDP_Growth = mean(GDP_Growth, na.rm = TRUE),
    Median_GDP_Growth = median(GDP_Growth, na.rm = TRUE),
    SD_GDP_Growth = sd(GDP_Growth, na.rm = TRUE),
    Min_GDP_Growth = min(GDP_Growth, na.rm = TRUE),
    Max_GDP_Growth = max(GDP_Growth, na.rm = TRUE)
  )
print(gdp_year_growth_summary)

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

plot_gdp_growth_year <- gdp_year_growth_summary %>%
  ggplot(aes(x = Year, y = Mean_GDP_Growth)) +
  geom_line(color = "green") +
  geom_point(color = "green") +
  labs(
    x = "Year",
    y = "Mean GDP Growth (%)",
    title = "Mean GDP Growth by Year"
  ) +
  theme_minimal()

plot_gdp_year
plot_gdp_growth_year

# Summary statistics for power plant data
power_plant_summary <- power_plants %>%
  mutate(commissioning_year = floor(commissioning_year)) %>%
  group_by(commissioning_year) %>%
  summarise(
    Total_Plants = n()
  )
print(power_plant_summary)

power_plant_fuel_summary <- power_plants %>%
  mutate(commissioning_year = floor(commissioning_year)) %>%
  group_by(commissioning_year, primary_fuel) %>%
  summarise(
    Total_Plants = n()
  )

plot1 <- power_plant_summary %>%
  ggplot(aes(x = commissioning_year, y = Total_Plants, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(
    x = "Commissioning Year",
    y = "Total Power Plants",
    title = "Total Power Plants by Commissioning Year"
  ) +
  theme_minimal()

plot1

plot2 <- power_plant_fuel_summary %>%
  ggplot(aes(x = commissioning_year, y = Total_Plants, color = primary_fuel, group = primary_fuel)) +
  geom_line() +
  geom_point() +
  labs(
    x = "Commissioning Year",
    y = "Total Power Plants",
    title = "Total Power Plants by Commissioning Year and Primary Fuel"
  ) +
  theme_minimal()

plot2
