library(tidyverse)
library(fixest)

# Load data
instrument_panel <- readRDS("Data/instrument_panel.rds") %>% 
  mutate(dist_to_urban_25k = as.numeric(dist_to_urban_25k),
         dist_to_urban_100k = as.numeric(dist_to_urban_100k),
         dist_to_urban_250k = as.numeric(dist_to_urban_250k),
         dist_to_urban_500k = as.numeric(dist_to_urban_500k),
         dist_to_urban_1mil = as.numeric(dist_to_urban_1mil))

inv_weighted_panel <- readRDS("Data/inv_weighted_panel.rds") %>% 
  mutate(dist_to_urban_25k = as.numeric(dist_to_urban_25k),
         dist_to_urban_100k = as.numeric(dist_to_urban_100k),
         dist_to_urban_250k = as.numeric(dist_to_urban_250k),
         dist_to_urban_500k = as.numeric(dist_to_urban_500k),
         dist_to_urban_1mil = as.numeric(dist_to_urban_1mil))

queen_weighted_panel <- readRDS("Data/queen_weighted_panel.rds") %>% 
  mutate(dist_to_urban_25k = as.numeric(dist_to_urban_25k),
         dist_to_urban_100k = as.numeric(dist_to_urban_100k),
         dist_to_urban_250k = as.numeric(dist_to_urban_250k),
         dist_to_urban_500k = as.numeric(dist_to_urban_500k),
         dist_to_urban_1mil = as.numeric(dist_to_urban_1mil))

rook_weighted_panel <- readRDS("Data/rook_weighted_panel.rds") %>% 
  mutate(dist_to_urban_25k = as.numeric(dist_to_urban_25k),
         dist_to_urban_100k = as.numeric(dist_to_urban_100k),
         dist_to_urban_250k = as.numeric(dist_to_urban_250k),
         dist_to_urban_500k = as.numeric(dist_to_urban_500k),
         dist_to_urban_1mil = as.numeric(dist_to_urban_1mil))

# Spatial Models
# Inverse Weights
inv_weighted_model <- feols(lnGDP_Per_Capita ~ cum_mw_wind + cum_mw_solar + cum_mw_gas + slag_cum_mw_wind + slag_cum_mw_solar + slag_cum_mw_gas + pct_white + pct_black + pct_poverty + pct_assoc + pct_bach + pct_masters + pct_farmer + pct_elderly + pct_under18 + pct_maleemploy + pct_femaleemploy + dist_to_urban_100k + dist_to_urban_250k + dist_to_urban_500k | GeoFIPS + Year, 
                                data = inv_weighted_panel, 
                                cluster = "Statefips")

# Queen Weights
queen_weighted_model <- feols(lnGDP_Per_Capita ~ cum_mw_wind + cum_mw_solar + cum_mw_gas + slag_cum_mw_wind + slag_cum_mw_solar + slag_cum_mw_gas + pct_white + pct_black + pct_poverty + pct_assoc + pct_bach + pct_masters + pct_farmer + pct_elderly + pct_under18 + pct_maleemploy + pct_femaleemploy + dist_to_urban_100k + dist_to_urban_250k + dist_to_urban_500k | GeoFIPS + Year, 
                                data = queen_weighted_panel, 
                                cluster = "Statefips")

# Rook Weights
rook_weighted_model <- feols(lnGDP_Per_Capita ~ cum_mw_wind + cum_mw_solar + cum_mw_gas + slag_cum_mw_wind + slag_cum_mw_solar + slag_cum_mw_gas + pct_white + pct_black + pct_poverty + pct_assoc + pct_bach + pct_masters + pct_farmer + pct_elderly + pct_under18 + pct_maleemploy + pct_femaleemploy + dist_to_urban_100k + dist_to_urban_250k + dist_to_urban_500k | GeoFIPS + Year, 
                                data = rook_weighted_panel, 
                                cluster = "Statefips")

# Basic Instrumented Model
fixest_FE_model_no_cluster_instrumented <- feols(lnGDP_Per_Capita ~ pct_white + pct_black + pct_poverty + 
                                       pct_assoc + pct_bach + pct_masters + pct_farmer + 
                                       pct_elderly + pct_under18 + pct_maleemploy + pct_femaleemploy + 
                                       dist_to_urban_100k + dist_to_urban_250k + dist_to_urban_500k | GeoFIPS + Year |
                                       cum_mw_wind + cum_mw_solar + cum_mw_gas ~ dni_cost_interaction + wind_cost_interaction + pipeline_cost_interaction, 
                                data = instrument_panel)

fixest_FE_model_state_cluster_instrumented <- feols(lnGDP_Per_Capita ~ pct_white + pct_black + pct_poverty + 
                                       pct_assoc + pct_bach + pct_masters + pct_farmer + 
                                       pct_elderly + pct_under18 + pct_maleemploy + pct_femaleemploy + 
                                       dist_to_urban_100k + dist_to_urban_250k + dist_to_urban_500k | GeoFIPS + Year |
                                       cum_mw_wind + cum_mw_solar + cum_mw_gas ~ dni_cost_interaction + wind_cost_interaction + pipeline_cost_interaction, 
                                data = instrument_panel, 
                                vcov = "cluster", 
                                cluster = "Statefips")

# Instrumented Spatial Models
# Inverse Weights
inv_weighted_model_instrumented <- feols(lnGDP_Per_Capita ~ pct_white + pct_black + pct_poverty + pct_assoc + pct_bach + pct_masters + pct_farmer + pct_elderly + pct_under18 + pct_maleemploy + pct_femaleemploy + dist_to_urban_100k + dist_to_urban_250k + dist_to_urban_500k | GeoFIPS + Year |
                                       cum_mw_wind + cum_mw_solar + cum_mw_gas + slag_cum_mw_wind + slag_cum_mw_solar + slag_cum_mw_gas ~ dni_cost_interaction + wind_cost_interaction + pipeline_cost_interaction + slag_dni_cost_interaction + slag_wind_cost_interaction + slag_pipeline_cost_interaction, 
                                data = inv_weighted_panel, 
                                cluster = "Statefips")

# Contiguity Weights
queen_weighted_model_instrumented <- feols(lnGDP_Per_Capita ~ pct_white + pct_black + pct_poverty + pct_assoc + pct_bach + pct_masters + pct_farmer + pct_elderly + pct_under18 + pct_maleemploy + pct_femaleemploy + dist_to_urban_100k + dist_to_urban_250k + dist_to_urban_500k | GeoFIPS + Year |
                                       cum_mw_wind + cum_mw_solar + cum_mw_gas + slag_cum_mw_wind + slag_cum_mw_solar + slag_cum_mw_gas ~ dni_cost_interaction + wind_cost_interaction + pipeline_cost_interaction + slag_dni_cost_interaction + slag_wind_cost_interaction + slag_pipeline_cost_interaction, 
                                data = queen_weighted_panel, 
                                cluster = "Statefips")
rook_weighted_model_instrumented <- feols(lnGDP_Per_Capita ~ pct_white + pct_black + pct_poverty + pct_assoc + pct_bach + pct_masters + pct_farmer + pct_elderly + pct_under18 + pct_maleemploy + pct_femaleemploy + dist_to_urban_100k + dist_to_urban_250k + dist_to_urban_500k | GeoFIPS + Year |
                                       cum_mw_wind + cum_mw_solar + cum_mw_gas + slag_cum_mw_wind + slag_cum_mw_solar + slag_cum_mw_gas ~ dni_cost_interaction + wind_cost_interaction + pipeline_cost_interaction + slag_dni_cost_interaction + slag_wind_cost_interaction + slag_pipeline_cost_interaction, 
                                data = rook_weighted_panel, 
                                cluster = "Statefips")

# Summarize results
summary(inv_weighted_model)
summary(queen_weighted_model)
summary(rook_weighted_model)
summary(fixest_FE_model_no_cluster_instrumented)
summary(fixest_FE_model_state_cluster_instrumented)
summary(inv_weighted_model_instrumented)
summary(queen_weighted_model_instrumented)
summary(rook_weighted_model_instrumented)

