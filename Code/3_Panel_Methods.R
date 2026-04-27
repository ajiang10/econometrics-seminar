library(tidyverse)
library(plm)

# Load panel
panel_data <- readRDS("Data/master_panel.rds") %>%
  filter(Year <= 2020) # For panel methods, we focus on 2002-2020 to avoid complications with post-treatment periods in the later years

# Run pooled OLS
Pooled_model <- plm(lnGDP_Per_Capita ~ new_mw_wind + new_mw_solar + new_mw_gas + pct_white + pct_black + pct_asian + pct_poverty + pct_assoc + pct_bach + pct_masters + pct_farmer + pct_elderly + pct_under18 + pct_maleemploy + pct_femaleemploy + natamen + dist_to_urban_25k + dist_to_urban_100k + dist_to_urban_250k + dist_to_urban_500k + dist_to_urban_1mil, 
                data = master_panel, 
                index = c("GeoFIPS", "Year"), 
                model = "pooling")
summary(Pooled_model)

# Run FE
FE_model <- plm(lnGDP_Per_Capita ~ new_mw_wind + new_mw_solar + new_mw_gas + pct_white + pct_black + pct_asian + pct_poverty + pct_assoc + pct_bach + pct_masters + pct_farmer + pct_elderly + pct_under18 + pct_maleemploy + pct_femaleemploy + natamen + dist_to_urban_25k + dist_to_urban_100k + dist_to_urban_250k + dist_to_urban_500k + dist_to_urban_1mil, 
                data = master_panel, 
                index = c("GeoFIPS", "Year"), 
                model = "within", 
                effect = "twoways")

summary(FE_model)


# Run RE
RE_model <- plm(lnGDP_Per_Capita ~ new_mw_wind + new_mw_solar + new_mw_gas + pct_white + pct_black + pct_asian + pct_poverty + pct_assoc + pct_bach + pct_masters + pct_farmer + pct_elderly + pct_under18 + pct_maleemploy + pct_femaleemploy + natamen + dist_to_urban_25k + dist_to_urban_100k + dist_to_urban_250k + dist_to_urban_500k + dist_to_urban_1mil, 
                data = master_panel, 
                index = c("GeoFIPS", "Year"), 
                model = "random", 
                effect = "twoways")
summary(RE_model)


# Tests for which model is best
plmtest(Pooled_model, type = "bp")
pFtest(FE_model, Pooled_model)
phtest(FE_model, RE_model)
