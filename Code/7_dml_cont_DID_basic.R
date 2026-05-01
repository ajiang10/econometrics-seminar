install.packages("tidyverse")
library(tidyverse)
install.packages("causalweight")
library(causalweight)

### IDEALLY ALL THIS WOULD BE RUN
### Due to hardware limitations, I was only able to run wind instanteous DIDs with LASSO.

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
wind_controls <- c("cum_mw_history_wind", "cum_mw_history_solar", 
                   "cum_mw_history_gas", "lag_mw_wind", 
                   "new_mw_solar", "new_mw_gas", 
                   "pct_white", "pct_black", 
                   "pct_native", "pct_asian", 
                   "pct_hawaiian", "pct_other", 
                   "pct_poverty", "pct_assoc", 
                   "pct_bach", "pct_masters", 
                   "pct_farmer", "pct_elderly", 
                   "pct_under18", "pct_maleemploy", 
                   "pct_femaleemploy", "natamen", 
                   "dist_to_urban_25k", "dist_to_urban_100k", 
                   "dist_to_urban_250k", "dist_to_urban_500k", 
                   "dist_to_urban_1mil", "Statefips", "ref_year")
solar_controls <- c("cum_mw_history_wind", "cum_mw_history_solar", 
                    "cum_mw_history_gas", "lag_mw_solar", 
                    "new_mw_wind", "new_mw_gas", 
                    "pct_white", "pct_black", 
                    "pct_native", "pct_asian", 
                    "pct_hawaiian", "pct_other", 
                    "pct_poverty", "pct_assoc", 
                    "pct_bach", "pct_masters", 
                    "pct_farmer", "pct_elderly", 
                    "pct_under18", "pct_maleemploy", 
                    "pct_femaleemploy", "natamen", 
                    "dist_to_urban_25k", "dist_to_urban_100k", 
                    "dist_to_urban_250k", "dist_to_urban_500k", 
                    "dist_to_urban_1mil", "Statefips", "ref_year")
gas_controls <- c("cum_mw_history_wind", "cum_mw_history_solar", 
                  "cum_mw_history_gas", "lag_mw_gas", 
                  "new_mw_wind", "new_mw_solar", 
                  "pct_white", "pct_black", 
                  "pct_native", "pct_asian", 
                  "pct_hawaiian", "pct_other", 
                  "pct_poverty", "pct_assoc", 
                  "pct_bach", "pct_masters", 
                  "pct_farmer", "pct_elderly", 
                  "pct_under18", "pct_maleemploy", 
                  "pct_femaleemploy", "natamen", 
                  "dist_to_urban_25k", "dist_to_urban_100k", 
                  "dist_to_urban_250k", "dist_to_urban_500k", 
                  "dist_to_urban_1mil", "Statefips", "ref_year")

# Instantaneous
# Wind
wind_instantaneous_did50lasso <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_wind,
  controls = instantaneous_stack %>% select(all_of(wind_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 0.5,           # Intensity: 50 MW added
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("wind_50_instant_done")
wind_50_instant_results <- data.frame(
  Model = "Lasso 50 MW",
  ATET = wind_instantaneous_did50lasso$ATET,
  SE = wind_instantaneous_did50lasso$se,
  P_Value = wind_instantaneous_did50lasso$pval
)
saveRDS(wind_50_instant_results, file = "wind_50_instant_results.rds") # Save individual result in case of later error
rm(wind_instantaneous_did50lasso) # memory management

wind_instantaneous_did100lasso <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_wind,
  controls = instantaneous_stack %>% select(all_of(wind_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 1,           # Intensity: 100 MW added
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("wind_100_instant_done")
wind_100_instant_results <- data.frame(
  Model = "Lasso 100 MW",
  ATET = wind_instantaneous_did100lasso$ATET,
  SE = wind_instantaneous_did100lasso$se,
  P_Value = wind_instantaneous_did100lasso$pval
)

saveRDS(wind_100_instant_results, file = "wind_100_instant_results.rds") # Save individual result in case of later error
rm(wind_instantaneous_did100lasso) # memory management

wind_instantaneous_did200lasso <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_wind,
  controls = instantaneous_stack %>% select(all_of(wind_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 2,           # Intensity: 200 MW added
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("wind_200_instant_done")
wind_200_instant_results <- data.frame(
  Model = "Lasso 200 MW",
  ATET = wind_instantaneous_did200lasso$ATET,
  SE = wind_instantaneous_did200lasso$se,
  P_Value = wind_instantaneous_did200lasso$pval
)
saveRDS(wind_200_instant_results, file = "wind_200_instant_results.rds") # Save individual result in case of later error
rm(wind_instantaneous_did200lasso) # memory management

# Wind Instantaneous results
wind_instantaneous_results <- rbind(wind_50_instant_results, wind_100_instant_results, wind_200_instant_results)

rm(list = c("wind_50_instant_results", 
            "wind_100_instant_results", 
            "wind_200_instant_results")) # memory management

# Solar
solar_instantaneous_did50lasso <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_solar,
  controls = instantaneous_stack %>% select(all_of(solar_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 0.5,           # Intensity: 50 MW added
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)

print("solar_50_instant_done")
solar_50_instant_results <- data.frame(
  Model = "Lasso 50 MW",
  ATET = solar_instantaneous_did50lasso$ATET,
  SE = solar_instantaneous_did50lasso$se,
  P_Value = solar_instantaneous_did50lasso$pval
)
saveRDS(solar_50_instant_results, file = "solar_50_instant_results.rds") # Save individual result in case of later error
rm(solar_instantaneous_did50lasso) # memory management


solar_instantaneous_did100lasso <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_solar,
  controls = instantaneous_stack %>% select(all_of(solar_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 1,           # Intensity: 100 MW added
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)

print("solar_100_instant_done")
solar_100_instant_results <- data.frame(
  Model = "Lasso 100 MW",
  ATET = solar_instantaneous_did100lasso$ATET,
  SE = solar_instantaneous_did100lasso$se,
  P_Value = solar_instantaneous_did100lasso$pval
)
saveRDS(solar_100_instant_results, file = "solar_100_instant_results.rds") # Save individual result in case of later error
rm(solar_instantaneous_did100lasso) # memory management

solar_instantaneous_did200lasso <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_solar,
  controls = instantaneous_stack %>% select(all_of(solar_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 2,           # Intensity: 200 MW added
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)

print("solar_200_instant_done")
solar_200_instant_results <- data.frame(
  Model = "Lasso 200 MW",
  ATET = solar_instantaneous_did200lasso$ATET,
  SE = solar_instantaneous_did200lasso$se,
  P_Value = solar_instantaneous_did200lasso$pval
)
saveRDS(solar_200_instant_results, file = "solar_200_instant_results.rds") # Save individual result in case of later error
rm(solar_instantaneous_did200lasso) # memory management


# Solar Instantaneous results
solar_instantaneous_results <- rbind(solar_50_instant_results, solar_100_instant_results, solar_200_instant_results)


rm(list = c("solar_50_instant_results", 
            "solar_100_instant_results", 
            "solar_200_instant_results")) # memory management

# Gas
gas_instantaneous_did50lasso <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_gas,
  controls = instantaneous_stack %>% select(all_of(gas_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 0.5,           # Intensity: 50 MW added
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("gas_50_instant_done")
gas_50_instant_results <- data.frame(
  Model = "Lasso 50 MW",
  ATET = gas_instantaneous_did50lasso$ATET,
  SE = gas_instantaneous_did50lasso$se,
  P_Value = gas_instantaneous_did50lasso$pval
)
saveRDS(gas_50_instant_results, file = "gas_50_instant_results.rds") # Save individual result in case of later error
rm(gas_instantaneous_did50lasso) # memory management

gas_instantaneous_did100lasso <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_gas,
  controls = instantaneous_stack %>% select(all_of(gas_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 1,           # Intensity: 100 MW added
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)

print("gas_100_instant_done")
gas_100_instant_results <- data.frame(
  Model = "Lasso 100 MW",
  ATET = gas_instantaneous_did100lasso$ATET,
  SE = gas_instantaneous_did100lasso$se,
  P_Value = gas_instantaneous_did100lasso$pval
)
saveRDS(gas_100_instant_results, file = "gas_100_instant_results.rds") # Save individual result in case of later error
rm(gas_instantaneous_did100lasso) # memory management

gas_instantaneous_did200lasso <- didcontDML(
  y = instantaneous_stack$lnGDP_Per_Capita, 
  d = instantaneous_stack$new_mw_gas,
  controls = instantaneous_stack %>% select(all_of(gas_controls)),
  t = instantaneous_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 2,           # Intensity: 200 MW added
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = instantaneous_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
        
print("gas_200_instant_done")
gas_200_instant_results <- data.frame(
  Model = "Lasso 200 MW",
  ATET = gas_instantaneous_did200lasso$ATET,
  SE = gas_instantaneous_did200lasso$se,
  P_Value = gas_instantaneous_did200lasso$pval
)
saveRDS(gas_200_instant_results, file = "gas_200_instant_results.rds") # Save individual result in case of later error
rm(gas_instantaneous_did200lasso) # memory management

# Gas Instantaneous results
gas_instantaneous_results <- rbind(gas_50_instant_results, gas_100_instant_results, gas_200_instant_results)

rm(list = c("gas_50_instant_results", 
            "gas_100_instant_results", 
            "gas_200_instant_results")) # memory management

# 1-year post ATET
# Wind
wind_one_year_post_did50lasso <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_wind,
  controls = one_year_post_stack %>% select(all_of(wind_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 0.5,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)

print("wind_50_one_year_post_done")
wind_50_one_year_post_results <- data.frame(
  Model = "Lasso 50 MW",
  ATET = wind_one_year_post_did50lasso$ATET,
  SE = wind_one_year_post_did50lasso$se,
  P_Value = wind_one_year_post_did50lasso$pval
)
saveRDS(wind_50_one_year_post_results, file = "wind_50_one_year_post_results.rds") # Save individual result in case of later error
rm(wind_one_year_post_did50lasso) # memory management

wind_one_year_post_did100lasso <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_wind,
  controls = one_year_post_stack %>% select(all_of(wind_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 1,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("wind_100_one_year_post_done")
wind_100_one_year_post_results <- data.frame(
  Model = "Lasso 100 MW",
  ATET = wind_one_year_post_did100lasso$ATET,
  SE = wind_one_year_post_did100lasso$se,
  P_Value = wind_one_year_post_did100lasso$pval
)
saveRDS(wind_100_one_year_post_results, file = "wind_100_one_year_post_results.rds") # Save individual result in case of later error
rm(wind_one_year_post_did100lasso) # memory management

wind_one_year_post_did200lasso <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_wind,
  controls = one_year_post_stack %>% select(all_of(wind_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 2,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)

print("wind_200_one_year_post_done")
wind_200_one_year_post_results <- data.frame(
  Model = "Lasso 200 MW",
  ATET = wind_one_year_post_did200lasso$ATET,
  SE = wind_one_year_post_did200lasso$se,
  P_Value = wind_one_year_post_did200lasso$pval
)
saveRDS(wind_200_one_year_post_results, file = "wind_200_one_year_post_results.rds") # Save individual result in case of later error
rm(wind_one_year_post_did200lasso) # memory management

# Wind One-Year Post results
wind_one_year_post_results <- rbind(wind_50_one_year_post_results, wind_100_one_year_post_results, wind_200_one_year_post_results)

rm(list = c("wind_50_one_year_post_results", 
            "wind_100_one_year_post_results", 
            "wind_200_one_year_post_results")) # memory management

# Solar
solar_one_year_post_did50lasso <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_solar,
  controls = one_year_post_stack %>% select(all_of(solar_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 0.5,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("solar_50_one_year_post_done")
solar_50_one_year_post_results <- data.frame(
  Model = "Lasso 50 MW",
  ATET = solar_one_year_post_did50lasso$ATET,
  SE = solar_one_year_post_did50lasso$se,
  P_Value = solar_one_year_post_did50lasso$pval
)
saveRDS(solar_50_one_year_post_results, file = "solar_50_one_year_post_results.rds") # Save individual result in case of later error
rm(solar_one_year_post_did50lasso) # memory management

solar_one_year_post_did100lasso <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_solar,
  controls = one_year_post_stack %>% select(all_of(solar_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 1,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("solar_100_one_year_post_done")
solar_100_one_year_post_results <- data.frame(
  Model = "Lasso 100 MW",
  ATET = solar_one_year_post_did100lasso$ATET,
  SE = solar_one_year_post_did100lasso$se,
  P_Value = solar_one_year_post_did100lasso$pval
)
saveRDS(solar_100_one_year_post_results, file = "solar_100_one_year_post_results.rds") # Save individual result in case of later error
rm(solar_one_year_post_did100lasso) # memory management

solar_one_year_post_did200lasso <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_solar,
  controls = one_year_post_stack %>% select(all_of(solar_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 0.2,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("solar_200_one_year_post_done")
solar_200_one_year_post_results <- data.frame(
  Model = "Lasso 200 MW",
  ATET = solar_one_year_post_did200lasso$ATET,
  SE = solar_one_year_post_did200lasso$se,
  P_Value = solar_one_year_post_did200lasso$pval
)
saveRDS(solar_200_one_year_post_results, file = "solar_200_one_year_post_results.rds") # Save individual result in case of later error
rm(solar_one_year_post_did200lasso) # memory management


# Solar One-Year Post results
solar_one_year_post_results <- rbind(solar_50_one_year_post_results, solar_100_one_year_post_results, solar_200_one_year_post_results)

rm(list = c("solar_50_one_year_post_results", 
            "solar_100_one_year_post_results", 
            "solar_200_one_year_post_results")) # memory management

# Gas 
gas_one_year_post_did50lasso <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_gas,
  controls = one_year_post_stack %>% select(all_of(gas_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 0.5,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("gas_50_one_year_post_done")
gas_50_one_year_post_results <- data.frame(
  Model = "Lasso 50 MW",
  ATET = gas_one_year_post_did50lasso$ATET,
  SE = gas_one_year_post_did50lasso$se,
  P_Value = gas_one_year_post_did50lasso$pval
)
saveRDS(gas_50_one_year_post_results, file = "gas_50_one_year_post_results.rds") # Save individual result in case of later error
rm(gas_one_year_post_did50lasso) # memory management

gas_one_year_post_did100lasso <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_gas,
  controls = one_year_post_stack %>% select(all_of(gas_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 1,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)

print("gas_100_one_year_post_done")
gas_100_one_year_post_results <- data.frame(
  Model = "Lasso 100 MW",
  ATET = gas_one_year_post_did100lasso$ATET,
  SE = gas_one_year_post_did100lasso$se,
  P_Value = gas_one_year_post_did100lasso$pval
)
saveRDS(gas_100_one_year_post_results, file = "gas_100_one_year_post_results.rds") # Save individual result in case of later error
rm(gas_one_year_post_did100lasso) # memory management

gas_one_year_post_did200lasso <- didcontDML(
  y = one_year_post_stack$lnGDP_Per_Capita, 
  d = one_year_post_stack$new_mw_gas,
  controls = one_year_post_stack %>% select(all_of(gas_controls)),
  t = one_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 2,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = one_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("gas_200_one_year_post_done")
gas_200_one_year_post_results <- data.frame(
  Model = "Lasso 200 MW",
  ATET = gas_one_year_post_did200lasso$ATET,
  SE = gas_one_year_post_did200lasso$se,
  P_Value = gas_one_year_post_did200lasso$pval
)
saveRDS(gas_200_one_year_post_results, file = "gas_200_one_year_post_results.rds") # Save individual result in case of later error
rm(gas_one_year_post_did200lasso) # memory management

# Gas One-Year Post results
gas_one_year_post_results <- rbind(gas_50_one_year_post_results, gas_100_one_year_post_results, gas_200_one_year_post_results)

rm(list = c("gas_50_one_year_post_results", 
            "gas_100_one_year_post_results", 
            "gas_200_one_year_post_results")) # memory management

# 2-year post ATET
# Wind
wind_two_year_post_did50lasso <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_wind,
  controls = two_year_post_stack %>% select(all_of(wind_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 0.5,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("wind_50_two_year_post_done")
wind_50_two_year_post_results <- data.frame(
  Model = "Lasso 50 MW",
  ATET = wind_two_year_post_did50lasso$ATET,
  SE = wind_two_year_post_did50lasso$se,
  P_Value = wind_two_year_post_did50lasso$pval
)
saveRDS(wind_50_two_year_post_results, file = "wind_50_two_year_post_results.rds") # Save individual result in case of later error
rm(wind_two_year_post_did50lasso) # memory management

wind_two_year_post_did100lasso <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_wind,
  controls = two_year_post_stack %>% select(all_of(wind_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 1,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)

print("wind_100_two_year_post_done")
wind_100_two_year_post_results <- data.frame(
  Model = "Lasso 100 MW",
  ATET = wind_two_year_post_did100lasso$ATET,
  SE = wind_two_year_post_did100lasso$se,
  P_Value = wind_two_year_post_did100lasso$pval
)
saveRDS(wind_100_two_year_post_results, file = "wind_100_two_year_post_results.rds") # Save individual result in case of later error
rm(wind_two_year_post_did100lasso) # memory management

wind_two_year_post_did200lasso <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_wind,
  controls = two_year_post_stack %>% select(all_of(wind_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 2,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)

print("wind_200_two_year_post_done")
wind_200_two_year_post_results <- data.frame(
  Model = "Lasso 200 MW",
  ATET = wind_two_year_post_did200lasso$ATET,
  SE = wind_two_year_post_did200lasso$se,
  P_Value = wind_two_year_post_did200lasso$pval
)
saveRDS(wind_200_two_year_post_results, file = "wind_200_two_year_post_results.rds") # Save individual result in case of later error
rm(wind_two_year_post_did200lasso) # memory management

# Wind Two-Year Post results
wind_two_year_post_results <- rbind(wind_50_two_year_post_results, wind_100_two_year_post_results, wind_200_two_year_post_results)

rm(list = c("wind_50_two_year_post_results", 
            "wind_100_two_year_post_results", 
            "wind_200_two_year_post_results")) # memory management

# Solar
solar_two_year_post_did50lasso <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_solar,
  controls = two_year_post_stack %>% select(all_of(solar_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 0.5,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("solar_50_two_year_post_done")
solar_50_two_year_post_results <- data.frame(
  Model = "Lasso 50 MW",
  ATET = solar_two_year_post_did50lasso$ATET,
  SE = solar_two_year_post_did50lasso$se,
  P_Value = solar_two_year_post_did50lasso$pval
)
saveRDS(solar_50_two_year_post_results, file = "solar_50_two_year_post_results.rds") # Save individual result in case of later error
rm(solar_two_year_post_did50lasso) # memory management

solar_two_year_post_did100lasso <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_solar,
  controls = two_year_post_stack %>% select(all_of(solar_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 1,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)

print("solar_100_two_year_post_done")
solar_100_two_year_post_results <- data.frame(
  Model = "Lasso 100 MW",
  ATET = solar_two_year_post_did100lasso$ATET,
  SE = solar_two_year_post_did100lasso$se,
  P_Value = solar_two_year_post_did100lasso$pval
)
saveRDS(solar_100_two_year_post_results, file = "solar_100_two_year_post_results.rds") # Save individual result in case of later error
rm(solar_two_year_post_did100lasso) # memory management

solar_two_year_post_did200lasso <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_solar,
  controls = two_year_post_stack %>% select(all_of(solar_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 2,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)

print("solar_200_two_year_post_done")
solar_200_two_year_post_results <- data.frame(
  Model = "Lasso 200 MW",
  ATET = solar_two_year_post_did200lasso$ATET,
  SE = solar_two_year_post_did200lasso$se,
  P_Value = solar_two_year_post_did200lasso$pval
)
saveRDS(solar_200_two_year_post_results, file = "solar_200_two_year_post_results.rds") # Save individual result in case of later error
rm(solar_two_year_post_did200lasso) # memory management

# Solar Two-Year Post results
solar_two_year_post_results <- rbind(solar_50_two_year_post_results, solar_100_two_year_post_results, solar_200_two_year_post_results)

rm(list = c("solar_50_two_year_post_results", 
            "solar_100_two_year_post_results", 
            "solar_200_two_year_post_results")) # memory management

# Gas
gas_two_year_post_did50lasso <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_gas,
  controls = two_year_post_stack %>% select(all_of(gas_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 0.5,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("gas_50_two_year_post_done")
gas_50_two_year_post_results <- data.frame(
  Model = "Lasso 50 MW",
  ATET = gas_two_year_post_did50lasso$ATET,
  SE = gas_two_year_post_did50lasso$se,
  P_Value = gas_two_year_post_did50lasso$pval
)
saveRDS(gas_50_two_year_post_results, file = "gas_50_two_year_post_results.rds") # Save individual result in case of later error
rm(gas_two_year_post_did50lasso) # memory management

gas_two_year_post_did100lasso <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_gas,
  controls = two_year_post_stack %>% select(all_of(gas_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 1,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("gas_100_two_year_post_done")
gas_100_two_year_post_results <- data.frame(
  Model = "Lasso 100 MW",
  ATET = gas_two_year_post_did100lasso$ATET,
  SE = gas_two_year_post_did100lasso$se,
  P_Value = gas_two_year_post_did100lasso$pval
)
saveRDS(gas_100_two_year_post_results, file = "gas_100_two_year_post_results.rds") # Save individual result in case of later error
rm(gas_two_year_post_did100lasso) # memory management

gas_two_year_post_did200lasso <- didcontDML(
  y = two_year_post_stack$lnGDP_Per_Capita, 
  d = two_year_post_stack$new_mw_gas,
  controls = two_year_post_stack %>% select(all_of(gas_controls)),
  t = two_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 2,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = two_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("gas_200_two_year_post_done")
gas_200_two_year_post_results <- data.frame(
  Model = "Lasso 200 MW",
  ATET = gas_two_year_post_did200lasso$ATET,
  SE = gas_two_year_post_did200lasso$se,
  P_Value = gas_two_year_post_did200lasso$pval
)
saveRDS(gas_200_two_year_post_results, file = "gas_200_two_year_post_results.rds") # Save individual result in case of later error
rm(gas_two_year_post_did200lasso) # memory management

# Gas Two-Year Post results
gas_two_year_post_results <- rbind(gas_50_two_year_post_results, gas_100_two_year_post_results, gas_200_two_year_post_results)

rm(list = c("gas_50_two_year_post_results", 
            "gas_100_two_year_post_results", 
            "gas_200_two_year_post_results")) # memory management

# 3-year post ATET
# Wind
wind_three_year_post_did50lasso <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_wind,
  controls = three_year_post_stack %>% select(all_of(wind_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 0.5,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("wind_50_three_year_post_done")
wind_50_three_year_post_results <- data.frame(
  Model = "Lasso 50 MW",
  ATET = wind_three_year_post_did50lasso$ATET,
  SE = wind_three_year_post_did50lasso$se,
  P_Value = wind_three_year_post_did50lasso$pval
)
saveRDS(wind_50_three_year_post_results, file = "wind_50_three_year_post_results.rds") # Save individual result in case of later error
rm(wind_three_year_post_did50lasso) # memory management

wind_three_year_post_did100lasso <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_wind,
  controls = three_year_post_stack %>% select(all_of(wind_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 1,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("wind_100_three_year_post_done")
wind_100_three_year_post_results <- data.frame(
  Model = "Lasso 100 MW",
  ATET = wind_three_year_post_did100lasso$ATET,
  SE = wind_three_year_post_did100lasso$se,
  P_Value = wind_three_year_post_did100lasso$pval
)
saveRDS(wind_100_three_year_post_results, file = "wind_100_three_year_post_results.rds") # Save individual result in case of later error
rm(wind_three_year_post_did100lasso) # memory management
wind_three_year_post_did200lasso <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_wind,
  controls = three_year_post_stack %>% select(all_of(wind_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 2,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("wind_200_three_year_post_done")
wind_200_three_year_post_results <- data.frame(
  Model = "Lasso 200 MW",
  ATET = wind_three_year_post_did200lasso$ATET,
  SE = wind_three_year_post_did200lasso$se,
  P_Value = wind_three_year_post_did200lasso$pval
)
saveRDS(wind_200_three_year_post_results, file = "wind_200_three_year_post_results.rds") # Save individual result in case of later error
rm(wind_three_year_post_did200lasso) # memory management

# Wind Three-Year Post results
wind_three_year_post_results <- rbind(wind_50_three_year_post_results, wind_100_three_year_post_results, wind_200_three_year_post_results)

rm(list = c("wind_50_three_year_post_results", 
            "wind_100_three_year_post_results", 
            "wind_200_three_year_post_results")) # memory management

# Solar
solar_three_year_post_did50lasso <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_solar,
  controls = three_year_post_stack %>% select(all_of(solar_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 0.5,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("solar_50_three_year_post_done")
solar_50_three_year_post_results <- data.frame(
  Model = "Lasso 50 MW",
  ATET = solar_three_year_post_did50lasso$ATET,
  SE = solar_three_year_post_did50lasso$se,
  P_Value = solar_three_year_post_did50lasso$pval
)
saveRDS(solar_50_three_year_post_results, file = "solar_50_three_year_post_results.rds") # Save individual result in case of later error
rm(solar_three_year_post_did50lasso) # memory management

solar_three_year_post_did100lasso <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_solar,
  controls = three_year_post_stack %>% select(all_of(solar_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 1,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("solar_100_three_year_post_done")
solar_100_three_year_post_results <- data.frame(
  Model = "Lasso 100 MW",
  ATET = solar_three_year_post_did100lasso$ATET,
  SE = solar_three_year_post_did100lasso$se,
  P_Value = solar_three_year_post_did100lasso$pval
)
saveRDS(solar_100_three_year_post_results, file = "solar_100_three_year_post_results.rds") # Save individual result in case of later error
rm(solar_three_year_post_did100lasso) # memory management

solar_three_year_post_did200lasso <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_solar,
  controls = three_year_post_stack %>% select(all_of(solar_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 2,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$Statefips, # Cluster standard errors at the state level
  k = 3          
)
print("solar_200_three_year_post_done")
solar_200_three_year_post_results <- data.frame(
  Model = "Lasso 200 MW",
  ATET = solar_three_year_post_did200lasso$ATET,
  SE = solar_three_year_post_did200lasso$se,
  P_Value = solar_three_year_post_did200lasso$pval
)
saveRDS(solar_200_three_year_post_results, file = "solar_200_three_year_post_results.rds") # Save individual result in case of later error
rm(solar_three_year_post_did200lasso) # memory management

# Solar Three-Year Post results
solar_three_year_post_results <- rbind(solar_50_three_year_post_results, solar_100_three_year_post_results, solar_200_three_year_post_results)

rm(list = c("solar_50_three_year_post_results", 
            "solar_100_three_year_post_results", 
            "solar_200_three_year_post_results")) # memory management

# Gas
gas_three_year_post_did50lasso <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_gas,
  controls = three_year_post_stack %>% select(all_of(gas_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 0.5,           # Intensity: 50 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$Statefips, # Cluster standard errors at the county level
  k = 3          
)
print("gas_50_three_year_post_done")
gas_50_three_year_post_results <- data.frame(
  Model = "Lasso 50 MW",
  ATET = gas_three_year_post_did50lasso$ATET,
  SE = gas_three_year_post_did50lasso$se,
  P_Value = gas_three_year_post_did50lasso$pval
)
saveRDS(gas_50_three_year_post_results, file = "gas_50_three_year_post_results.rds") # Save individual result in case of later error
rm(gas_three_year_post_did50lasso) # memory management

gas_three_year_post_did100lasso <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_gas,
  controls = three_year_post_stack %>% select(all_of(gas_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 1,           # Intensity: 100 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$Statefips, # Cluster standard errors at the county level
  k = 3          
)
print("gas_100_three_year_post_done")
gas_100_three_year_post_results <- data.frame(
  Model = "Lasso 100 MW",
  ATET = gas_three_year_post_did100lasso$ATET,
  SE = gas_three_year_post_did100lasso$se,
  P_Value = gas_three_year_post_did100lasso$pval
)
saveRDS(gas_100_three_year_post_results, file = "gas_100_three_year_post_results.rds") # Save individual result in case of later error
rm(gas_three_year_post_did100lasso) # memory management
gas_three_year_post_did200lasso <- didcontDML(
  y = three_year_post_stack$lnGDP_Per_Capita, 
  d = three_year_post_stack$new_mw_gas,
  controls = three_year_post_stack %>% select(all_of(gas_controls)),
  t = three_year_post_stack$time,
  dcontrol = 0,             # Comparison: 0 MW added
  dtreat = 2,           # Intensity: 200 MW added
  t0 = 0,
  t1 = 2,
  MLmethod = "lasso", # Or "randomforest" for non-linear history effects
  cluster = three_year_post_stack$Statefips, # Cluster standard errors at the county level
  k = 3          
)
print("gas_200_three_year_post_done")
gas_200_three_year_post_results <- data.frame(
  Model = "Lasso 200 MW",
  ATET = gas_three_year_post_did200lasso$ATET,
  SE = gas_three_year_post_did200lasso$se,
  P_Value = gas_three_year_post_did200lasso$pval
)
saveRDS(gas_200_three_year_post_results, file = "gas_200_three_year_post_results.rds") # Save individual result in case of later error
rm(gas_three_year_post_did200lasso) # memory management

# Gas Three-Year Post results
gas_three_year_post_results <- rbind(gas_50_three_year_post_results, gas_100_three_year_post_results, gas_200_three_year_post_results)

rm(list = c("gas_50_three_year_post_results", 
            "gas_100_three_year_post_results", 
            "gas_200_three_year_post_results")) # memory management

# Results
instantaneous_results <- rbind(wind_instantaneous_results, solar_instantaneous_results, gas_instantaneous_results)
one_year_post_results <- rbind(wind_one_year_post_results, solar_one_year_post_results, gas_one_year_post_results)
two_year_post_results <- rbind(wind_two_year_post_results, solar_two_year_post_results, gas_two_year_post_results)
three_year_post_results <- rbind(wind_three_year_post_results, solar_three_year_post_results, gas_three_year_post_results)

full_results <- rbind(instantaneous_results, one_year_post_results, two_year_post_results, three_year_post_results)
full_results$Time_Period <- rep(c("Instantaneous", "One Year Post", "Two Year Post", "Three Year Post"), each = 9)

# Save results
save(instantaneous_results, one_year_post_results, two_year_post_results, three_year_post_results, full_results, file = "dml_results.RData")
saveRDS(full_results, "full_results.rds")