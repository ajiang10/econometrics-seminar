library(tidyverse)
library(plm)
library(fixest)
library(carr)

# Load panel
master_panel <- readRDS("Data/master_panel.rds") %>%
  filter(Year <= 2020) %>% # For panel methods, we focus on 2002-2020 to avoid complications with post-treatment periods in the later years
  mutate(dist_to_urban_25k = as.numeric(dist_to_urban_25k),
         dist_to_urban_100k = as.numeric(dist_to_urban_100k),
         dist_to_urban_250k = as.numeric(dist_to_urban_250k),
         dist_to_urban_500k = as.numeric(dist_to_urban_500k),
         dist_to_urban_1mil = as.numeric(dist_to_urban_1mil)) # Convert distance variables to numeric for regression
# Run FE
FE_model <- plm(lnGDP_Per_Capita ~ cum_mw_wind + cum_mw_solar + cum_mw_gas + pct_white + pct_black + pct_poverty + pct_assoc + pct_bach + pct_masters + pct_farmer + pct_elderly + pct_under18 + pct_maleemploy + pct_femaleemploy + dist_to_urban_100k + dist_to_urban_250k + dist_to_urban_500k,
                data = master_panel, 
                index = c("GeoFIPS", "Year"), 
                model = "within", 
                effect = "twoways") # For later testing, we'll use Fixest to easily obtain clustered SEs

fixest_FE_model_no_cluster <- feols(lnGDP_Per_Capita ~ cum_mw_wind + cum_mw_solar + cum_mw_gas + pct_white + pct_black + pct_poverty + pct_assoc + pct_bach + pct_masters + pct_farmer + pct_elderly + pct_under18 + pct_maleemploy + pct_femaleemploy + dist_to_urban_100k + dist_to_urban_250k + dist_to_urban_500k | GeoFIPS + Year, 
                                data = master_panel)
fixest_FE_model_state_cluster <- feols(lnGDP_Per_Capita ~ cum_mw_wind + cum_mw_solar + cum_mw_gas + pct_white + pct_black + pct_poverty + pct_assoc + pct_bach + pct_masters + pct_farmer + pct_elderly + pct_under18 + pct_maleemploy + pct_femaleemploy + dist_to_urban_100k + dist_to_urban_250k + dist_to_urban_500k | GeoFIPS + Year, 
                                data = master_panel, 
                                cluster = "Statefips")

summary(fixest_FE_model_no_cluster)
summary(fixest_FE_model_state_cluster)

# Lets check if a FE model is actually appropriate
# Run pooled OLS
Pooled_model <- plm(lnGDP_Per_Capita ~ cum_mw_wind + cum_mw_solar + cum_mw_gas + pct_white + pct_black + pct_poverty + pct_assoc + pct_bach + pct_masters + pct_farmer + pct_elderly + pct_under18 + pct_maleemploy + pct_femaleemploy + dist_to_urban_100k + dist_to_urban_250k + dist_to_urban_500k,
                data = master_panel, 
                index = c("GeoFIPS", "Year"), 
                model = "pooling")
summary(Pooled_model)

# Run RE
RE_model <- plm(lnGDP_Per_Capita ~ cum_mw_wind + cum_mw_solar + cum_mw_gas + pct_white + pct_black + pct_poverty + pct_assoc + pct_bach + pct_masters + pct_farmer + pct_elderly + pct_under18 + pct_maleemploy + pct_femaleemploy  + dist_to_urban_100k + dist_to_urban_250k + dist_to_urban_500k,
                data = master_panel, 
                index = c("GeoFIPS", "Year"), 
                model = "random", 
                effect = "twoways")
summary(RE_model)


# Tests for which model is best
plmtest(Pooled_model, type = "bp")
pFtest(FE_model, Pooled_model)
phtest(FE_model, RE_model)

