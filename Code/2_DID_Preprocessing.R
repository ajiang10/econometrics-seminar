library(tidyverse)
library(sf)

# Load data
county_panel <- readRDS("Data/cleaned_county_panel.rds")
power_plants <- readRDS("Data/cleaned_power_plant_data.rds") %>%
  st_drop_geometry()
geofips_adjacency <- readRDS("Data/geofips_adjacency.rds")

# Treatment data
treatment_annual_wind <- power_plants %>%
  filter(primary_fuel == "Wind") %>%
  group_by(GeoFIPS, Year = commissioning_year) %>%
  summarise(new_mw_wind = sum(capacity_mw, na.rm = TRUE), .groups = "drop") %>%
  group_by(GeoFIPS) %>%
  mutate(cum_mw_wind_pre = sum(new_mw_wind[Year < 2002], na.rm = TRUE)) %>%
  mutate(lag_mw_wind = case_when(Year == 2002 ~ lag(new_mw_wind, default = 0))) %>%
  ungroup() %>%
  filter(Year >= 2002)

treatment_annual_solar <- power_plants %>%
  filter(primary_fuel == "Solar") %>%
  group_by(GeoFIPS, Year = commissioning_year) %>%
  summarise(new_mw_solar = sum(capacity_mw, na.rm = TRUE), .groups = "drop") %>%
  group_by(GeoFIPS) %>%
  mutate(cum_mw_solar_pre = sum(new_mw_solar[Year < 2002], na.rm = TRUE)) %>%
  mutate(lag_mw_solar = case_when(Year == 2002 ~ lag(new_mw_solar, default = 0))) %>%
  ungroup() %>%
  filter(Year >= 2002)

treatment_annual_gas <- power_plants %>%
  filter(primary_fuel == "Gas") %>%
  group_by(GeoFIPS, Year = commissioning_year) %>%
  summarise(new_mw_gas = sum(capacity_mw, na.rm = TRUE), .groups = "drop") %>%
  group_by(GeoFIPS) %>%
  mutate(cum_mw_gas_pre = sum(new_mw_gas[Year < 2002], na.rm = TRUE)) %>%
  mutate(lag_mw_gas = case_when(Year == 2002 ~ lag(new_mw_gas, default = 0))) %>%
  ungroup() %>%
  filter(Year >= 2002)

# Create master panel
master_panel <- county_panel %>%
  left_join(treatment_annual_wind, by = c("GeoFIPS", "Year")) %>%
  left_join(treatment_annual_solar, by = c("GeoFIPS", "Year")) %>%
  left_join(treatment_annual_gas, by = c("GeoFIPS", "Year")) %>%
  mutate(across(c(new_mw_wind, new_mw_solar, new_mw_gas,
                  cum_mw_wind_pre, cum_mw_solar_pre, cum_mw_gas_pre,
                  lag_mw_wind, lag_mw_solar, lag_mw_gas),
                ~ replace_na(.x, 0))) %>%
  group_by(GeoFIPS) %>%
  arrange(Year, .by_group = TRUE) %>%
  mutate(
    cum_mw_history_wind  = cum_mw_wind_pre  + accumulate(lag(new_mw_wind,  default = 0), `+`),
    cum_mw_history_solar = cum_mw_solar_pre + accumulate(lag(new_mw_solar, default = 0), `+`),
    cum_mw_history_gas   = cum_mw_gas_pre   + accumulate(lag(new_mw_gas,   default = 0), `+`)
  ) %>%
  ungroup() %>%
  select(-c(cum_mw_wind_pre, cum_mw_solar_pre, cum_mw_gas_pre)) # Drop pre-period cumulative columns as they are now incorporated into the history variables

# Save master panel
saveRDS(master_panel, "Data/master_panel.rds")

# Stack Data for didcontDML function
start_yr <- 2003
end_yr <- 2020
# Instantaneous
instantaneous_stack <- data.frame()

for (t in start_yr:end_yr) {
  # Extract the two-period panel for this 'stack' (t-1 and t)
  stack_t <- master_panel %>%
    filter(Year %in% c(t-1, t)) %>%
    mutate(
      time = ifelse(Year == t, 1, 0),
      stack_id = paste0(GeoFIPS, "_", t), # Unique ID for this unit-period pair
      ref_year = as.factor(t)             # Dummy variable for the stack
    ) %>%
    # Ensure we have both periods for the unit
    group_by(stack_id) %>%
    filter(n() == 2) %>%
    ungroup()
  
  instantaneous_stack <- rbind(instantaneous_stack, stack_t)
}

# Save the instantaneous stack for later use
saveRDS(instantaneous_stack, "Data/instantaneous_stack.rds")

# 1-year post
one_year_post_stack <- data.frame()

for (t in start_yr:end_yr) {
  # Extract the three-period panel for this 'stack' (t-1, t, and t+1)
  stack_t <- master_panel %>%
    filter(Year %in% c(t-1, t, t+1)) %>%
    mutate(
      time = case_when(
        Year == t   ~ 1,
        Year == t+1 ~ 2,
        Year == t-1 ~ 0
      ),
      stack_id = paste0(GeoFIPS, "_", t), # Unique ID for this unit-period pair
      ref_year = as.factor(t)             # Dummy variable for the stack
    ) %>%
    # Ensure we have both periods for the unit
    group_by(stack_id) %>%
    filter(n() == 3) %>%
    mutate(
      new_mw_wind  = replace(new_mw_wind,  time == 2, new_mw_wind[time == 1]),
      new_mw_solar = replace(new_mw_solar, time == 2, new_mw_solar[time == 1]),
      new_mw_gas   = replace(new_mw_gas,   time == 2, new_mw_gas[time == 1])
    ) %>%
    ungroup()

  one_year_post_stack <- rbind(one_year_post_stack, stack_t)
}

# Save the 1-year post stack for later use
saveRDS(one_year_post_stack, "Data/one_year_post_stack.rds")

# 2-year post
two_year_post_stack <- data.frame()

for (t in start_yr:end_yr) {
  # Extract the three-period panel for this 'stack' (t-1, t, and t+2)
  stack_t <- master_panel %>%
    filter(Year %in% c(t-1, t, t+2)) %>%
    mutate(
      time = case_when(
        Year == t   ~ 1,
        Year == t+2 ~ 2,
        Year == t-1 ~ 0
      ),
      stack_id = paste0(GeoFIPS, "_", t), # Unique ID for this unit-period pair
      ref_year = as.factor(t)             # Dummy variable for the stack
    ) %>%
    # Ensure we have both periods for the unit
    group_by(stack_id) %>%
    filter(n() == 3) %>%
    mutate(
      new_mw_wind  = replace(new_mw_wind,  time == 2, new_mw_wind[time == 1]),
      new_mw_solar = replace(new_mw_solar, time == 2, new_mw_solar[time == 1]),
      new_mw_gas   = replace(new_mw_gas,   time == 2, new_mw_gas[time == 1])
    ) %>%
    ungroup()

  two_year_post_stack <- rbind(two_year_post_stack, stack_t)
}

# Save the 2-year post stack for later use
saveRDS(two_year_post_stack, "Data/two_year_post_stack.rds")

# 3-year post
three_year_post_stack <- data.frame()

for (t in start_yr:end_yr) {
  # Extract the three-period panel for this 'stack' (t-1, t, and t+3)
  stack_t <- master_panel %>%
    filter(Year %in% c(t-1, t, t+3)) %>%
    mutate(
      time = case_when(
        Year == t   ~ 1,
        Year == t+3 ~ 2,
        Year == t-1 ~ 0
      ),
      stack_id = paste0(GeoFIPS, "_", t), # Unique ID for this unit-period pair
      ref_year = as.factor(t)             # Dummy variable for the stack
    ) %>%
    # Ensure we have both periods for the unit
    group_by(stack_id) %>%
    filter(n() == 3) %>%
    mutate(
      new_mw_wind  = replace(new_mw_wind,  time == 2, new_mw_wind[time == 1]),
      new_mw_solar = replace(new_mw_solar, time == 2, new_mw_solar[time == 1]),
      new_mw_gas   = replace(new_mw_gas,   time == 2, new_mw_gas[time == 1])
    ) %>%
    ungroup()
  
  three_year_post_stack <- rbind(three_year_post_stack, stack_t)
}

# Save the 3-year post stack for later use
saveRDS(three_year_post_stack, "Data/three_year_post_stack.rds")
