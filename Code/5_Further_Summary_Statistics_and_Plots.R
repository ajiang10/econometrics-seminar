library(tidyverse)
library(ggplot2)
library(sf)
library(terra)
library(tigris)
library(units)

# Load data
wind_tif_resampled <- rast("Data/USA_wind-speed_100m_resampled.tif")
solar_tif <- rast("Data/nsrdb3_dni.tif")
us_counties <- counties(class = "sf", year = 2019) %>%
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")) # Exclude Alaska, Hawaii, and territories
us_counties <- st_transform(us_counties, crs = 4326) # For compatibility with solar, wind, and gas shapefiles
us_countiesEPSG <- st_transform(us_counties, crs = 5070) # Albers Equal Area for accurate distance calculations
power_plants <- readRDS("Data/cleaned_power_plant_data.rds")

# Get average values for solar and wind by county
average_county_solar <- extract(solar_tif, us_counties, fun = mean, na.rm = TRUE, bind = TRUE)
average_county_wind <- extract(wind_tif_resampled, us_counties, fun = mean, na.rm = TRUE, bind = TRUE)

average_county_solar <- as.data.frame(average_county_solar) %>%
  select(GEOID, nsrdb3_dni) %>%
  rename(avg_solar_dni = nsrdb3_dni)

average_county_wind <- as.data.frame(average_county_wind) %>%
  select(GEOID, USA_wind.speed_100m) %>%
  rename(avg_wind_speed_100m = USA_wind.speed_100m)

# Load gas pipeline shapefile
natural_gas_shapefile <- st_read("Data/Natural_Gas_Interstate_and_Intrastate_Pipelines/Natural_Gas_Interstate_and_Intrastate_Pipelines.shp") %>%
  st_transform(crs = 5070) # Albers Equal Area for accurate distance calculations

# Calculate distance from each county centroid to nearest natural gas pipeline
county_centroids <- st_centroid(us_countiesEPSG)


nearest_pipeline_idx <- st_nearest_feature(county_centroids, natural_gas_shapefile)
nearest_pipeline_distance <- st_distance(
  county_centroids, 
  natural_gas_shapefile[nearest_pipeline_idx, ], 
  by_element = TRUE
) %>%
  set_units("mi")

us_counties <- us_counties %>%
  mutate(nearest_pipeline_distance = as.numeric(nearest_pipeline_distance)) %>%
  left_join(average_county_solar, by = c("GEOID" = "GEOID")) %>%
  left_join(average_county_wind, by = c("GEOID" = "GEOID"))

saveRDS(us_counties, "Data/us_counties_with_renewable_and_pipeline_data.rds")

# Plot Maps of Each variable
wind_100m_plot <- ggplot(us_counties) +
  geom_sf(aes(fill = avg_wind_speed_100m), color = "grey70", size = 0.15) +
  geom_sf(
    data = power_plants %>% filter(primary_fuel == "Wind"),
    color = "blue", shape = 1, size = 0.5, inherit.aes = FALSE
  ) +
  scale_fill_viridis_c(option = "plasma", na.value = "transparent", name = "Avg Wind Speed (m/s)") +
  labs(title = "Average Wind Speed at 100m by County") +
  theme_void(base_size = 12) +
  theme(text = element_text(family = "Linux Libertine"), plot.title = element_text(hjust = 1/2, face = "bold", size = 16))

solar_dni_plot <- ggplot(us_counties) +
  geom_sf(aes(fill = avg_solar_dni), color = "grey70", size = 0.15) +
  geom_sf(
    data = power_plants %>% filter(primary_fuel == "Solar"),
    color = "green", shape = 1, size = 0.5, inherit.aes = FALSE
  ) +
  scale_fill_viridis_c(option = "plasma", na.value = "transparent", name = "Avg Solar DNI (W/m²)") +
  labs(title = "Average Solar DNI by County") +
  theme_void(base_size = 12) +
  theme(text = element_text(family = "Linux Libertine"), plot.title = element_text(hjust = 1/2, face = "bold", size = 16))

pipeline_distance_plot <- ggplot() +
  geom_sf(data = us_counties, aes(fill = nearest_pipeline_distance), color = "black") +
  geom_sf(
    data = power_plants %>% filter(primary_fuel == "Gas"), 
    color = "red", shape = 1, size = 0.5, inherit.aes = FALSE
  ) +
  scale_fill_viridis_c(option = "plasma", na.value = "transparent", name = "Distance to Pipeline (mi)") +
  labs(title = "Distance to Nearest Natural Gas Pipeline by County") +
  theme_void(base_size = 12) +
  theme(text = element_text(family = "Linux Libertine"), plot.title = element_text(hjust = 1/2, face = "bold", size = 16))

ggsave(
  filename = "Output/wind_speed_100m.png",
  plot = wind_100m_plot,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

ggsave(
  filename = "Output/solar_dni.png",
  plot = solar_dni_plot,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

ggsave(
  filename = "Output/pipeline_distance.png",
  plot = pipeline_distance_plot,
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)

