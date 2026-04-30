library(tidyverse)
library(sf)
library(spdep)
library(tigris)
library(terra)
library(units)
library(lubridate)
library(plm)
library(splm)

# Load master panel
master_panel <- readRDS("Data/master_panel.rds") %>%
  filter(Year <= 2020) # For panel methods, we focus on 2002-2020 to avoid complications with post-treatment periods in the later years

# Load county shapefile from tigiris
us_counties <- counties(class = "sf", year = 2019) %>%
  filter(!STATEFP %in% c("02", "15", "51", "60", "66", "69", "72", "78")) # Exclude Alaska, Hawaii, Viriginia, and territories
us_counties <- st_transform(us_counties, crs = 4326) # For compatibility with solar, wind, and gas shapefiles
us_countiesEPSG <- st_transform(us_counties, crs = 5070) # Albers Equal Area for accurate distance calculations

# Load solar and wind geotiffs
solar_tif <- rast("Data/nsrdb3_dni.tif")
wind_tif <- rast("Data/USA_wind-speed_100m.tif")
wind_tif_resampled <- resample(wind_tif, solar_tif, method = "bilinear") # Resample wind to match solar resolution and extent

writeRaster(wind_tif_resampled, "Data/USA_wind-speed_100m_resampled.tif", overwrite = TRUE) # Save resampled wind raster for future use
# Can reload it to save time in future runs
# wind_tif <- rast("Data/USA_wind-speed_100m_resampled.tif")

# Calculate average solar and wind by county
average_county_solar <- extract(solar_tif, us_counties, fun = mean, na.rm = TRUE, bind = TRUE)
average_county_wind <- extract(wind_tif_resampled, us_counties, fun = mean, na.rm = TRUE, bind = TRUE)

average_county_solar <- as.data.frame(average_county_solar) %>%
  select(GEOID, nsrdb3_dni) %>%
  rename(avg_solar_dni = nsrdb3_dni)

average_county_wind <- as.data.frame(average_county_wind) %>%
  select(GEOID, USA_wind.speed_100m) %>%
  rename(avg_wind_speed_100m = USA_wind.speed_100m)

saveRDS(average_county_solar, "Data/average_county_solar.rds")
saveRDS(average_county_wind, "Data/average_county_wind.rds")

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
  mutate(nearest_pipeline_distance = nearest_pipeline_distance)

# Load cost data
solar_cost_data <- read_csv("Data/solar-pv-prices.csv") %>%
  select(Year, `Solar PV module cost`) %>%
  filter(Year >= 2002 & Year <= 2020) %>%
  rename(Solar_Cost = `Solar PV module cost`)
wind_cost_data <- data.frame("Year" = 2002:2020, "Wind_Cost" = c(2030, 1844, 1700, 1996, 2318, 2451, 2777, 3004, 2996, 2886, 2611, 2572, 2250, 2072, 2041, 2010, 1770, 1722, 1758)) # manually extracted from xlsx
natural_gas_cost_data <- read_csv("Data/PCU333611333611.csv") %>%
  mutate(observation_date = as.Date(observation_date)) %>%
  mutate(Year = year(observation_date)) %>%
  select(Year, PCU333611333611) %>%
  filter(Year >= 2002 & Year <= 2020) %>%
  rename(Natural_Gas_Cost = PCU333611333611)


instrumental_variables <- expand.grid(GeoFIPS = unique(master_panel$GeoFIPS), Year = 2002:2020) %>%
  left_join(average_county_solar, by = c("GeoFIPS" = "GEOID")) %>%
  left_join(average_county_wind, by = c("GeoFIPS" = "GEOID")) %>%
  left_join(us_counties %>% st_drop_geometry() %>% select(GEOID, nearest_pipeline_distance), by = c("GeoFIPS" = "GEOID")) %>%
  left_join(solar_cost_data, by = "Year") %>%
  left_join(wind_cost_data, by = "Year") %>%
  left_join(natural_gas_cost_data, by = "Year") %>%
  mutate(nearest_pipeline_distance = as.numeric(nearest_pipeline_distance)) %>%
  mutate(dni_cost_interaction = avg_solar_dni * Solar_Cost,
         wind_cost_interaction = avg_wind_speed_100m * Wind_Cost,
         pipeline_cost_interaction = nearest_pipeline_distance * Natural_Gas_Cost)

# Add newly created instrumental variables to master panel
instrument_panel <- master_panel %>%
  left_join(instrumental_variables, by = c("GeoFIPS" = "GeoFIPS", "Year" = "Year"))
saveRDS(instrument_panel, "Data/instrument_panel.rds") # Save so we don't have to repeat the merging and variable creation steps in future runs

# Create spatial weight matrix for spatial regression
county_centroids_coords <- st_coordinates(county_centroids)

fips_order <- sort(unique(master_panel$GeoFIPS))
us_countiesEPSG  <- us_countiesEPSG %>%
  arrange(match(GEOID, fips_order))

# Easy ones (contiguity-based)
nb_queen <- poly2nb(us_countiesEPSG, queen = TRUE)
W_queen_contiguity <- nb2mat(nb_queen, style = "W", zero.policy = TRUE)
nb_rook <- poly2nb(us_countiesEPSG, queen = FALSE)
W_rook_contiguity <- nb2mat(nb_rook, style = "W", zero.policy = TRUE)



# Distance-based weight matrix (e.g., inverse distance)
# Build k-nearest-neighbour object to get distances, then threshold
dist_obj <- dnearneigh(county_centroids_coords, d1 = 0, d2 = 200000)   # 200 km cutoff (metres)
dlist    <- nbdists(dist_obj, county_centroids_coords)                   # raw distances
 
# Inverse-distance, row-normalised
inv_dist_weights <- lapply(dlist, function(d) 1 / d)
W_invdist <- nb2mat(dist_obj, glist = inv_dist_weights, style = "W",
                    zero.policy = TRUE)

save(nb_queen, W_queen_contiguity, nb_rook, W_rook_contiguity, W_invdist, file = "Data/spatial_weights.RData") # Save so no need to recompute in future runs

# Make weight matrices into listw objects for use in splm
weights_object_queen <- mat2listw(W_queen_contiguity, style = "W", zero.policy = TRUE)
weights_object_rook <- mat2listw(W_rook_contiguity, style = "W", zero.policy = TRUE)
weights_object_invdist <- mat2listw(W_invdist, style = "W", zero.policy = TRUE)

# Calculate spatially weighted cumulative MW and instruments
# Inverse distance weighted panel
inv_weighted_panel <- pdata.frame(instrument_panel, index = c("GeoFIPS", "Year"))
inv_weighted_panel$slag_cum_mw_wind <- slag(inv_weighted_panel$cum_mw_wind, listw = weights_object_invdist)
inv_weighted_panel$slag_cum_mw_solar <- slag(inv_weighted_panel$cum_mw_solar, listw = weights_object_invdist)
inv_weighted_panel$slag_cum_mw_gas <- slag(inv_weighted_panel$cum_mw_gas, listw = weights_object_invdist)
inv_weighted_panel$slag_dni_cost_interaction <- slag(inv_weighted_panel$dni_cost_interaction, listw = weights_object_invdist)
inv_weighted_panel$slag_wind_cost_interaction <- slag(inv_weighted_panel$wind_cost_interaction, listw = weights_object_invdist)
inv_weighted_panel$slag_pipeline_cost_interaction <- slag(inv_weighted_panel$pipeline_cost_interaction, listw = weights_object_invdist)

saveRDS(inv_weighted_panel, "Data/inv_weighted_panel.rds") # Save so we don't have to repeat the spatial lag calculations in future runs

# Queen contiguity weighted panel
queen_weighted_panel <- pdata.frame(instrument_panel, index = c("GeoFIPS", "Year"))
queen_weighted_panel$slag_cum_mw_wind <- slag(queen_weighted_panel$cum_mw_wind, listw = weights_object_queen)
queen_weighted_panel$slag_cum_mw_solar <- slag(queen_weighted_panel$cum_mw_solar, listw = weights_object_queen)
queen_weighted_panel$slag_cum_mw_gas <- slag(queen_weighted_panel$cum_mw_gas, listw = weights_object_queen)
queen_weighted_panel$slag_dni_cost_interaction <- slag(queen_weighted_panel$dni_cost_interaction, listw = weights_object_queen)
queen_weighted_panel$slag_wind_cost_interaction <- slag(queen_weighted_panel$wind_cost_interaction, listw = weights_object_queen)
queen_weighted_panel$slag_pipeline_cost_interaction <- slag(queen_weighted_panel$pipeline_cost_interaction, listw = weights_object_queen) 

saveRDS(queen_weighted_panel, "Data/queen_weighted_panel.rds") # Save so we don't have to repeat the spatial lag calculations in future runs

# Rook contiguity weighted panel
rook_weighted_panel <- pdata.frame(instrument_panel, index = c("GeoFIPS", "Year"))
rook_weighted_panel$slag_cum_mw_wind <- slag(rook_weighted_panel$cum_mw_wind, listw = weights_object_rook)
rook_weighted_panel$slag_cum_mw_solar <- slag(rook_weighted_panel$cum_mw_solar, listw = weights_object_rook)
rook_weighted_panel$slag_cum_mw_gas <- slag(rook_weighted_panel$cum_mw_gas, listw = weights_object_rook)
rook_weighted_panel$slag_dni_cost_interaction <- slag(rook_weighted_panel$dni_cost_interaction, listw = weights_object_rook)
rook_weighted_panel$slag_wind_cost_interaction <- slag(rook_weighted_panel$wind_cost_interaction, listw = weights_object_rook)
rook_weighted_panel$slag_pipeline_cost_interaction <- slag(rook_weighted_panel$pipeline_cost_interaction, listw = weights_object_rook)

saveRDS(rook_weighted_panel, "Data/rook_weighted_panel.rds") # Save so we don't have to repeat the spatial lag calculations in future runs