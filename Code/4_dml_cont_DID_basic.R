install.packages("tidyverse")
library(tidyverse)
install.packages("causalweight")
library(causalweight)

# Load stacked data
instantaneous_stack <- readRDS("Data/instantaneous_stack.rds") %>%
  mutate(Statefips = as.factor(Statefips), GeoFIPS = as.factor(GeoFIPS))
one_year_post_stack <- readRDS("Data/one_year_post_stack.rds") %>%
  mutate(Statefips = as.factor(Statefips), GeoFIPS = as.factor(GeoFIPS))
two_year_post_stack <- readRDS("Data/two_year_post_stack.rds") %>%
  mutate(Statefips = as.factor(Statefips), GeoFIPS = as.factor(GeoFIPS))
three_year_post_stack <- readRDS("Data/three_year_post_stack.rds") %>%
  mutate(Statefips = as.factor(Statefips), GeoFIPS = as.factor(GeoFIPS))

# Define controls for each stack
wind_controls <- c("cum_mw_history_wind", "cum_mw_history_solar", "cum_mw_history_gas", "lag_mw_wind", "new_mw_solar", "new_mw_gas", "pct_white", "pct_black", "pct_native", "pct_asian", "pct_hawaiian", "pct_other", "pct_poverty", "pct_assoc", "pct_bach", "pct_masters", "pct_farmer", "pct_elderly", "pct_under18", "pct_maleemploy", "pct_femaleemploy", "natamen", "dist_to_urban_25k", "dist_to_urban_100k", "dist_to_urban_250k", "dist_to_urban_500k", "dist_to_urban_1mil", "Statefips", "ref_year")
solar_controls <- c("cum_mw_history_wind", "cum_mw_history_solar", "cum_mw_history_gas", "lag_mw_solar", "new_mw_wind", "new_mw_gas", "pct_white", "pct_black", "pct_native", "pct_asian", "pct_hawaiian", "pct_other", "pct_poverty", "pct_assoc", "pct_bach", "pct_masters", "pct_farmer", "pct_elderly", "pct_under18", "pct_maleemploy", "pct_femaleemploy", "natamen", "dist_to_urban_25k", "dist_to_urban_100k", "dist_to_urban_250k", "dist_to_urban_500k", "dist_to_urban_1mil", "Statefips", "ref_year")
gas_controls <- c("cum_mw_history_wind", "cum_mw_history_solar", "cum_mw_history_gas", "lag_mw_gas", "new_mw_wind", "new_mw_solar", "pct_white", "pct_black", "pct_native", "pct_asian", "pct_hawaiian", "pct_other", "pct_poverty", "pct_assoc", "pct_bach", "pct_masters", "pct_farmer", "pct_elderly", "pct_under18", "pct_maleemploy", "pct_femaleemploy", "natamen", "dist_to_urban_25k", "dist_to_urban_100k", "dist_to_urban_250k", "dist_to_urban_500k", "dist_to_urban_1mil", "Statefips", "ref_year")

# Instantaneous
# Wind
wind_instantaneous_did50lasso <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_wind,
  controls = instantaneous_stack %>% select(all_of(wind_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
wind_instantaneous_did100lasso <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_wind,
  controls = instantaneous_stack %>% select(all_of(wind_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
wind_instantaneous_did200lasso <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_wind,
  controls = instantaneous_stack %>% select(all_of(wind_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)

wind_instantaneous_did50randomforest <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_wind,
  controls = instantaneous_stack %>% select(all_of(wind_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)

wind_instantaneous_did100randomforest <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_wind,
  controls = instantaneous_stack %>% select(all_of(wind_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)

wind_instantaneous_did200randomforest <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_wind,
  controls = instantaneous_stack %>% select(all_of(wind_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)

# Wind Instantaneous results
wind_instantaneous_results <- data.frame(
  Model = c("Lasso 50 MW", "Lasso 100 MW", "Lasso 200 MW", "Random Forest 50 MW", "Random Forest 100 MW", "Random Forest 200 MW"),
  ATET = c(wind_instantaneous_did50lasso$ATET, 
           wind_instantaneous_did100lasso$ATET, 
           wind_instantaneous_did200lasso$ATET, 
           wind_instantaneous_did50randomforest$ATET, 
           wind_instantaneous_did100randomforest$ATET, 
           wind_instantaneous_did200randomforest$ATET),
  SE = c(wind_instantaneous_did50lasso$se, 
         wind_instantaneous_did100lasso$se, 
         wind_instantaneous_did200lasso$se, 
         wind_instantaneous_did50randomforest$se, 
         wind_instantaneous_did100randomforest$se, 
         wind_instantaneous_did200randomforest$se),
  P_Value = c(wind_instantaneous_did50lasso$pval, 
              wind_instantaneous_did100lasso$pval, 
              wind_instantaneous_did200lasso$pval, 
              wind_instantaneous_did50randomforest$pval, 
              wind_instantaneous_did100randomforest$pval, 
              wind_instantaneous_did200randomforest$pval)
)

rm(list = c("wind_instantaneous_did50lasso", "wind_instantaneous_did100lasso", "wind_instantaneous_did200lasso", "wind_instantaneous_did50randomforest", "wind_instantaneous_did100randomforest", "wind_instantaneous_did200randomforest")) # memory management

# Solar
solar_instantaneous_did50lasso <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_solar,
  controls = instantaneous_stack %>% select(all_of(solar_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
solar_instantaneous_did100lasso <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_solar,
  controls = instantaneous_stack %>% select(all_of(solar_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
solar_instantaneous_did200lasso <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_solar,
  controls = instantaneous_stack %>% select(all_of(solar_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)

solar_instantaneous_did50randomforest <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_solar,
  controls = instantaneous_stack %>% select(all_of(solar_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
solar_instantaneous_did100randomforest <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_solar,
  controls = instantaneous_stack %>% select(all_of(solar_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
solar_instantaneous_did200randomforest <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_solar,
  controls = instantaneous_stack %>% select(all_of(solar_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
# Solar Instantaneous results
solar_instantaneous_results <- data.frame(
  Model = c("Lasso 50 MW", "Lasso 100 MW", "Lasso 200 MW", "Random Forest 50 MW", "Random Forest 100 MW", "Random Forest 200 MW"),
  ATET = c(solar_instantaneous_did50lasso$ATET, solar_instantaneous_did100lasso$ATET, solar_instantaneous_did200lasso$ATET, solar_instantaneous_did50randomforest$ATET, solar_instantaneous_did100randomforest$ATET, solar_instantaneous_did200randomforest$ATET),
  SE = c(solar_instantaneous_did50lasso$se, solar_instantaneous_did100lasso$se, solar_instantaneous_did200lasso$se, solar_instantaneous_did50randomforest$se, solar_instantaneous_did100randomforest$se, solar_instantaneous_did200randomforest$se),
  P_Value = c(solar_instantaneous_did50lasso$pval, solar_instantaneous_did100lasso$pval, solar_instantaneous_did200lasso$pval, solar_instantaneous_did50randomforest$pval, solar_instantaneous_did100randomforest$pval, solar_instantaneous_did200randomforest$pval)
)

rm(list = c("solar_instantaneous_did50lasso", "solar_instantaneous_did100lasso", "solar_instantaneous_did200lasso", "solar_instantaneous_did50randomforest", "solar_instantaneous_did100randomforest", "solar_instantaneous_did200randomforest")) # memory management

# Gas
gas_instantaneous_did50lasso <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_gas,
  controls = instantaneous_stack %>% select(all_of(gas_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
gas_instantaneous_did100lasso <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_gas,
  controls = instantaneous_stack %>% select(all_of(gas_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
gas_instantaneous_did200lasso <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_gas,
  controls = instantaneous_stack %>% select(all_of(gas_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)

gas_instantaneous_did50randomforest <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_gas,
  controls = instantaneous_stack %>% select(all_of(gas_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
gas_instantaneous_did100randomforest <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_gas,
  controls = instantaneous_stack %>% select(all_of(gas_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
gas_instantaneous_did200randomforest <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_gas,
  controls = instantaneous_stack %>% select(all_of(gas_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
# Gas Instantaneous results
gas_instantaneous_results <- data.frame(
  Model = c("Lasso 50 MW", "Lasso 100 MW", "Lasso 200 MW", "Random Forest 50 MW", "Random Forest 100 MW", "Random Forest 200 MW"),
  ATET = c(gas_instantaneous_did50lasso$ATET, gas_instantaneous_did100lasso$ATET, gas_instantaneous_did200lasso$ATET, gas_instantaneous_did50randomforest$ATET, gas_instantaneous_did100randomforest$ATET, gas_instantaneous_did200randomforest$ATET),
  SE = c(gas_instantaneous_did50lasso$se, gas_instantaneous_did100lasso$se, gas_instantaneous_did200lasso$se, gas_instantaneous_did50randomforest$se, gas_instantaneous_did100randomforest$se, gas_instantaneous_did200randomforest$se),
  P_Value = c(gas_instantaneous_did50lasso$pval, gas_instantaneous_did100lasso$pval, gas_instantaneous_did200lasso$pval, gas_instantaneous_did50randomforest$pval, gas_instantaneous_did100randomforest$pval, gas_instantaneous_did200randomforest$pval)
)

rm(list = c("gas_instantaneous_did50lasso", "gas_instantaneous_did100lasso", "gas_instantaneous_did200lasso", "gas_instantaneous_did50randomforest", "gas_instantaneous_did100randomforest", "gas_instantaneous_did200randomforest")) # memory management

# 1-year post ATET
# Wind
wind_one_year_post_did50lasso <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_wind,
  controls = one_year_post_stack %>% select(all_of(wind_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
wind_one_year_post_did100lasso <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_wind,
  controls = one_year_post_stack %>% select(all_of(wind_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
wind_one_year_post_did200lasso <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_wind,
  controls = one_year_post_stack %>% select(all_of(wind_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)

wind_one_year_post_did50randomforest <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_wind,
  controls = one_year_post_stack %>% select(all_of(wind_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
wind_one_year_post_did100randomforest <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_wind,
  controls = one_year_post_stack %>% select(all_of(wind_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
wind_one_year_post_did200randomforest <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_wind,
  controls = one_year_post_stack %>% select(all_of(wind_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
# Wind One-Year Post results
wind_one_year_post_results <- data.frame(
  Model = c("Lasso 50 MW", "Lasso 100 MW", "Lasso 200 MW", "Random Forest 50 MW", "Random Forest 100 MW", "Random Forest 200 MW"),
  ATET = c(wind_one_year_post_did50lasso$ATET, wind_one_year_post_did100lasso$ATET, wind_one_year_post_did200lasso$ATET, wind_one_year_post_did50randomforest$ATET, wind_one_year_post_did100randomforest$ATET, wind_one_year_post_did200randomforest$ATET),
  SE = c(wind_one_year_post_did50lasso$se, wind_one_year_post_did100lasso$se, wind_one_year_post_did200lasso$se, wind_one_year_post_did50randomforest$se, wind_one_year_post_did100randomforest$se, wind_one_year_post_did200randomforest$se),
  P_Value = c(wind_one_year_post_did50lasso$pval, wind_one_year_post_did100lasso$pval, wind_one_year_post_did200lasso$pval, wind_one_year_post_did50randomforest$pval, wind_one_year_post_did100randomforest$pval, wind_one_year_post_did200randomforest$pval)
)

rm(list = c("wind_one_year_post_did50lasso", "wind_one_year_post_did100lasso", "wind_one_year_post_did200lasso", "wind_one_year_post_did50randomforest", "wind_one_year_post_did100randomforest", "wind_one_year_post_did200randomforest")) # memory management

# Solar
solar_one_year_post_did50lasso <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_solar,
  controls = one_year_post_stack %>% select(all_of(solar_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
solar_one_year_post_did100lasso <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_solar,
  controls = one_year_post_stack %>% select(all_of(solar_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
solar_one_year_post_did200lasso <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_solar,
  controls = one_year_post_stack %>% select(all_of(solar_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)

solar_one_year_post_did50randomforest <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_solar,
  controls = one_year_post_stack %>% select(all_of(solar_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
solar_one_year_post_did100randomforest <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_solar,
  controls = one_year_post_stack %>% select(all_of(solar_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
solar_one_year_post_did200randomforest <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_solar,
  controls = one_year_post_stack %>% select(all_of(solar_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
# Solar One-Year Post results
solar_one_year_post_results <- data.frame(
  Model = c("Lasso 50 MW", "Lasso 100 MW", "Lasso 200 MW", "Random Forest 50 MW", "Random Forest 100 MW", "Random Forest 200 MW"),
  ATET = c(solar_one_year_post_did50lasso$ATET, solar_one_year_post_did100lasso$ATET, solar_one_year_post_did200lasso$ATET, solar_one_year_post_did50randomforest$ATET, solar_one_year_post_did100randomforest$ATET, solar_one_year_post_did200randomforest$ATET),
  SE = c(solar_one_year_post_did50lasso$se, solar_one_year_post_did100lasso$se, solar_one_year_post_did200lasso$se, solar_one_year_post_did50randomforest$se, solar_one_year_post_did100randomforest$se, solar_one_year_post_did200randomforest$se),
  P_Value = c(solar_one_year_post_did50lasso$pval, solar_one_year_post_did100lasso$pval, solar_one_year_post_did200lasso$pval, solar_one_year_post_did50randomforest$pval, solar_one_year_post_did100randomforest$pval, solar_one_year_post_did200randomforest$pval)
)

rm(list = c("solar_one_year_post_did50lasso", "solar_one_year_post_did100lasso", "solar_one_year_post_did200lasso", "solar_one_year_post_did50randomforest", "solar_one_year_post_did100randomforest", "solar_one_year_post_did200randomforest")) # memory management

# Gas 
gas_one_year_post_did50lasso <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_gas,
  controls = one_year_post_stack %>% select(all_of(gas_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
gas_one_year_post_did100lasso <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_gas,
  controls = one_year_post_stack %>% select(all_of(gas_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
gas_one_year_post_did200lasso <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_gas,
  controls = one_year_post_stack %>% select(all_of(gas_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)

gas_one_year_post_did50randomforest <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_gas,
  controls = one_year_post_stack %>% select(all_of(gas_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
gas_one_year_post_did100randomforest <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_gas,
  controls = one_year_post_stack %>% select(all_of(gas_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
gas_one_year_post_did200randomforest <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_gas,
  controls = one_year_post_stack %>% select(all_of(gas_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
# Gas One-Year Post results
gas_one_year_post_results <- data.frame(
  Model = c("Lasso 50 MW", "Lasso 100 MW", "Lasso 200 MW", "Random Forest 50 MW", "Random Forest 100 MW", "Random Forest 200 MW"),
  ATET = c(gas_one_year_post_did50lasso$ATET, gas_one_year_post_did100lasso$ATET, gas_one_year_post_did200lasso$ATET, gas_one_year_post_did50randomforest$ATET, gas_one_year_post_did100randomforest$ATET, gas_one_year_post_did200randomforest$ATET),
  SE = c(gas_one_year_post_did50lasso$se, gas_one_year_post_did100lasso$se, gas_one_year_post_did200lasso$se, gas_one_year_post_did50randomforest$se, gas_one_year_post_did100randomforest$se, gas_one_year_post_did200randomforest$se),
  P_Value = c(gas_one_year_post_did50lasso$pval, gas_one_year_post_did100lasso$pval, gas_one_year_post_did200lasso$pval, gas_one_year_post_did50randomforest$pval, gas_one_year_post_did100randomforest$pval, gas_one_year_post_did200randomforest$pval)
)

rm(list = c("gas_one_year_post_did50lasso", "gas_one_year_post_did100lasso", "gas_one_year_post_did200lasso", "gas_one_year_post_did50randomforest", "gas_one_year_post_did100randomforest", "gas_one_year_post_did200randomforest")) # memory management

# 2-year post ATET
# Wind
wind_two_year_post_did50lasso <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_wind,
  controls = two_year_post_stack %>% select(all_of(wind_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
wind_two_year_post_did100lasso <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_wind,
  controls = two_year_post_stack %>% select(all_of(wind_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
wind_two_year_post_did200lasso <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_wind,
  controls = two_year_post_stack %>% select(all_of(wind_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)

wind_two_year_post_did50randomforest <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_wind,
  controls = two_year_post_stack %>% select(all_of(wind_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
wind_two_year_post_did100randomforest <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_wind,
  controls = two_year_post_stack %>% select(all_of(wind_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
wind_two_year_post_did200randomforest <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_wind,
  controls = two_year_post_stack %>% select(all_of(wind_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
# Wind Two-Year Post results
wind_two_year_post_results <- data.frame(
  Model = c("Lasso 50 MW", "Lasso 100 MW", "Lasso 200 MW", "Random Forest 50 MW", "Random Forest 100 MW", "Random Forest 200 MW"),
  ATET = c(wind_two_year_post_did50lasso$ATET, wind_two_year_post_did100lasso$ATET, wind_two_year_post_did200lasso$ATET, wind_two_year_post_did50randomforest$ATET, wind_two_year_post_did100randomforest$ATET, wind_two_year_post_did200randomforest$ATET),
  SE = c(wind_two_year_post_did50lasso$se, wind_two_year_post_did100lasso$se, wind_two_year_post_did200lasso$se, wind_two_year_post_did50randomforest$se, wind_two_year_post_did100randomforest$se, wind_two_year_post_did200randomforest$se),
  P_Value = c(wind_two_year_post_did50lasso$pval, wind_two_year_post_did100lasso$pval, wind_two_year_post_did200lasso$pval, wind_two_year_post_did50randomforest$pval, wind_two_year_post_did100randomforest$pval, wind_two_year_post_did200randomforest$pval)
)

rm(list = c("wind_two_year_post_did50lasso", "wind_two_year_post_did100lasso", "wind_two_year_post_did200lasso", "wind_two_year_post_did50randomforest", "wind_two_year_post_did100randomforest", "wind_two_year_post_did200randomforest")) # memory management

# Solar
solar_two_year_post_did50lasso <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_solar,
  controls = two_year_post_stack %>% select(all_of(solar_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
solar_two_year_post_did100lasso <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_solar,
  controls = two_year_post_stack %>% select(all_of(solar_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
solar_two_year_post_did200lasso <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_solar,
  controls = two_year_post_stack %>% select(all_of(solar_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)

solar_two_year_post_did50randomforest <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_solar,
  controls = two_year_post_stack %>% select(all_of(solar_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
solar_two_year_post_did100randomforest <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_solar,
  controls = two_year_post_stack %>% select(all_of(solar_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
solar_two_year_post_did200randomforest <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_solar,
  controls = two_year_post_stack %>% select(all_of(solar_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
# Solar Two-Year Post results
solar_two_year_post_results <- data.frame(
  Model = c("Lasso 50 MW", "Lasso 100 MW", "Lasso 200 MW", "Random Forest 50 MW", "Random Forest 100 MW", "Random Forest 200 MW"),
  ATET = c(solar_two_year_post_did50lasso$ATET, solar_two_year_post_did100lasso$ATET, solar_two_year_post_did200lasso$ATET, solar_two_year_post_did50randomforest$ATET, solar_two_year_post_did100randomforest$ATET, solar_two_year_post_did200randomforest$ATET),
  SE = c(solar_two_year_post_did50lasso$se, solar_two_year_post_did100lasso$se, solar_two_year_post_did200lasso$se, solar_two_year_post_did50randomforest$se, solar_two_year_post_did100randomforest$se, solar_two_year_post_did200randomforest$se),
  P_Value = c(solar_two_year_post_did50lasso$pval, solar_two_year_post_did100lasso$pval, solar_two_year_post_did200lasso$pval, solar_two_year_post_did50randomforest$pval, solar_two_year_post_did100randomforest$pval, solar_two_year_post_did200randomforest$pval)
)

rm(list = c("solar_two_year_post_did50lasso", "solar_two_year_post_did100lasso", "solar_two_year_post_did200lasso", "solar_two_year_post_did50randomforest", "solar_two_year_post_did100randomforest", "solar_two_year_post_did200randomforest")) # memory management

# Gas
gas_two_year_post_did50lasso <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_gas,
  controls = two_year_post_stack %>% select(all_of(gas_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
gas_two_year_post_did100lasso <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_gas,
  controls = two_year_post_stack %>% select(all_of(gas_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
gas_two_year_post_did200lasso <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_gas,
  controls = two_year_post_stack %>% select(all_of(gas_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)

gas_two_year_post_did50randomforest <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_gas,
  controls = two_year_post_stack %>% select(all_of(gas_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
gas_two_year_post_did100randomforest <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_gas,
  controls = two_year_post_stack %>% select(all_of(gas_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
gas_two_year_post_did200randomforest <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_gas,
  controls = two_year_post_stack %>% select(all_of(gas_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
# Gas Two-Year Post results
gas_two_year_post_results <- data.frame(
  Model = c("Lasso 50 MW", "Lasso 100 MW", "Lasso 200 MW", "Random Forest 50 MW", "Random Forest 100 MW", "Random Forest 200 MW"),
  ATET = c(gas_two_year_post_did50lasso$ATET, gas_two_year_post_did100lasso$ATET, gas_two_year_post_did200lasso$ATET, gas_two_year_post_did50randomforest$ATET, gas_two_year_post_did100randomforest$ATET, gas_two_year_post_did200randomforest$ATET),
  SE = c(gas_two_year_post_did50lasso$se, gas_two_year_post_did100lasso$se, gas_two_year_post_did200lasso$se, gas_two_year_post_did50randomforest$se, gas_two_year_post_did100randomforest$se, gas_two_year_post_did200randomforest$se),
  P_Value = c(gas_two_year_post_did50lasso$pval, gas_two_year_post_did100lasso$pval, gas_two_year_post_did200lasso$pval, gas_two_year_post_did50randomforest$pval, gas_two_year_post_did100randomforest$pval, gas_two_year_post_did200randomforest$pval)
)

rm(list = c("gas_two_year_post_did50lasso", "gas_two_year_post_did100lasso", "gas_two_year_post_did200lasso", "gas_two_year_post_did50randomforest", "gas_two_year_post_did100randomforest", "gas_two_year_post_did200randomforest")) # memory management

# 3-year post ATET
# Wind
wind_three_year_post_did50lasso <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_wind,
  controls = three_year_post_stack %>% select(all_of(wind_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
wind_three_year_post_did100lasso <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_wind,
  controls = three_year_post_stack %>% select(all_of(wind_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
wind_three_year_post_did200lasso <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_wind,
  controls = three_year_post_stack %>% select(all_of(wind_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)

wind_three_year_post_did50randomforest <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_wind,
  controls = three_year_post_stack %>% select(all_of(wind_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
wind_three_year_post_did100randomforest <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_wind,
  controls = three_year_post_stack %>% select(all_of(wind_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
wind_three_year_post_did200randomforest <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_wind,
  controls = three_year_post_stack %>% select(all_of(wind_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
# Wind Three-Year Post results
wind_three_year_post_results <- data.frame(
  Model = c("Lasso 50 MW", "Lasso 100 MW", "Lasso 200 MW", "Random Forest 50 MW", "Random Forest 100 MW", "Random Forest 200 MW"),
  ATET = c(wind_three_year_post_did50lasso$ATET, wind_three_year_post_did100lasso$ATET, wind_three_year_post_did200lasso$ATET, wind_three_year_post_did50randomforest$ATET, wind_three_year_post_did100randomforest$ATET, wind_three_year_post_did200randomforest$ATET),
  SE = c(wind_three_year_post_did50lasso$se, wind_three_year_post_did100lasso$se, wind_three_year_post_did200lasso$se, wind_three_year_post_did50randomforest$se, wind_three_year_post_did100randomforest$se, wind_three_year_post_did200randomforest$se),
  P_Value = c(wind_three_year_post_did50lasso$pval, wind_three_year_post_did100lasso$pval, wind_three_year_post_did200lasso$pval, wind_three_year_post_did50randomforest$pval, wind_three_year_post_did100randomforest$pval, wind_three_year_post_did200randomforest$pval)
)

rm(list = c("wind_three_year_post_did50lasso", "wind_three_year_post_did100lasso", "wind_three_year_post_did200lasso", "wind_three_year_post_did50randomforest", "wind_three_year_post_did100randomforest", "wind_three_year_post_did200randomforest")) # memory management

# Solar
solar_three_year_post_did50lasso <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_solar,
  controls = three_year_post_stack %>% select(all_of(solar_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
solar_three_year_post_did100lasso <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_solar,
  controls = three_year_post_stack %>% select(all_of(solar_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
solar_three_year_post_did200lasso <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_solar,
  controls = three_year_post_stack %>% select(all_of(solar_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)

solar_three_year_post_did50randomforest <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_solar,
  controls = three_year_post_stack %>% select(all_of(solar_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
solar_three_year_post_did100randomforest <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_solar,
  controls = three_year_post_stack %>% select(all_of(solar_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
solar_three_year_post_did200randomforest <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_solar,
  controls = three_year_post_stack %>% select(all_of(solar_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
# Solar Three-Year Post results
solar_three_year_post_results <- data.frame(
  Model = c("Lasso 50 MW", "Lasso 100 MW", "Lasso 200 MW", "Random Forest 50 MW", "Random Forest 100 MW", "Random Forest 200 MW"),
  ATET = c(solar_three_year_post_did50lasso$ATET, solar_three_year_post_did100lasso$ATET, solar_three_year_post_did200lasso$ATET, solar_three_year_post_did50randomforest$ATET, solar_three_year_post_did100randomforest$ATET, solar_three_year_post_did200randomforest$ATET),
  SE = c(solar_three_year_post_did50lasso$se, solar_three_year_post_did100lasso$se, solar_three_year_post_did200lasso$se, solar_three_year_post_did50randomforest$se, solar_three_year_post_did100randomforest$se, solar_three_year_post_did200randomforest$se),
  P_Value = c(solar_three_year_post_did50lasso$pval, solar_three_year_post_did100lasso$pval, solar_three_year_post_did200lasso$pval, solar_three_year_post_did50randomforest$pval, solar_three_year_post_did100randomforest$pval, solar_three_year_post_did200randomforest$pval)
)

rm(list = c("solar_three_year_post_did50lasso", "solar_three_year_post_did100lasso", "solar_three_year_post_did200lasso", "solar_three_year_post_did50randomforest", "solar_three_year_post_did100randomforest", "solar_three_year_post_did200randomforest"))

# Gas
gas_three_year_post_did50lasso <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_gas,
  controls = three_year_post_stack %>% select(all_of(gas_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
gas_three_year_post_did100lasso <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_gas,
  controls = three_year_post_stack %>% select(all_of(gas_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
gas_three_year_post_did200lasso <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_gas,
  controls = three_year_post_stack %>% select(all_of(gas_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)

gas_three_year_post_did50randomforest <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_gas,
  controls = three_year_post_stack %>% select(all_of(gas_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 50,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
gas_three_year_post_did100randomforest <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_gas,
  controls = three_year_post_stack %>% select(all_of(gas_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 100,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
gas_three_year_post_did200randomforest <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_gas,
  controls = three_year_post_stack %>% select(all_of(gas_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 200,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "randomforest", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$GeoFIPS, # Cluster standard errors at the county level
  k = 3          
)
# Gas Three-Year Post results
gas_three_year_post_results <- data.frame(
  Model = c("Lasso 50 MW", "Lasso 100 MW", "Lasso 200 MW", "Random Forest 50 MW", "Random Forest 100 MW", "Random Forest 200 MW"),
  ATET = c(gas_three_year_post_did50lasso$ATET, gas_three_year_post_did100lasso$ATET, gas_three_year_post_did200lasso$ATET, gas_three_year_post_did50randomforest$ATET, gas_three_year_post_did100randomforest$ATET, gas_three_year_post_did200randomforest$ATET),
  SE = c(gas_three_year_post_did50lasso$se, gas_three_year_post_did100lasso$se, gas_three_year_post_did200lasso$se, gas_three_year_post_did50randomforest$se, gas_three_year_post_did100randomforest$se, gas_three_year_post_did200randomforest$se),
  P_Value = c(gas_three_year_post_did50lasso$pval, gas_three_year_post_did100lasso$pval, gas_three_year_post_did200lasso$pval, gas_three_year_post_did50randomforest$pval, gas_three_year_post_did100randomforest$pval, gas_three_year_post_did200randomforest$pval)
)

rm(list = c("gas_three_year_post_did50lasso", "gas_three_year_post_did100lasso", "gas_three_year_post_did200lasso", "gas_three_year_post_did50randomforest", "gas_three_year_post_did100randomforest", "gas_three_year_post_did200randomforest"))

# Results
instantaneous_results <- rbind(wind_instantaneous_results, solar_instantaneous_results, gas_instantaneous_results)
one_year_post_results <- rbind(wind_one_year_post_results, solar_one_year_post_results, gas_one_year_post_results)
two_year_post_results <- rbind(wind_two_year_post_results, solar_two_year_post_results, gas_two_year_post_results)
three_year_post_results <- rbind(wind_three_year_post_results, solar_three_year_post_results, gas_three_year_post_results)

full_results <- rbind(instantaneous_results, one_year_post_results, two_year_post_results, three_year_post_results)

# Save results
save(instantaneous_results, one_year_post_results, two_year_post_results, three_year_post_results, full_results, file = "dml_results.RData")
saveRDS(full_results, "full_results.rds")