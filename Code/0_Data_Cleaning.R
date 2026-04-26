# Libraries
library(tidyverse)
library(tidycensus)
library(sf)
library(tigris)
library(readxl)
library(units)

# Set Census API Key (will need to obtain your own key and insert it here)
# census_api_key("API_KEY_HERE", install = TRUE)

# Load Data
global_power_plant_data <- read_csv("Data/global_power_plant_database.csv")

# Load the geofips adjacency data
geofips <- read_delim("Data/county_adjacency2010.txt", col_names = F) %>%
  rename("County_Name" = X1, "County_GEOID" = X2, "Adjacent_County_Name" = X3, "Adjacent_County_GEOID" = X4) %>%
  mutate(County_Name = replace(County_Name, County_GEOID == "35013", "Dona Ana County, NM")) %>%
  mutate(Adjacent_County_Name = replace(Adjacent_County_Name, Adjacent_County_GEOID == "35013", "Dona Ana County, NM")) %>%
  fill(County_Name, County_GEOID, .direction = "down")

# Save geofips adjacency data for later use
write_csv(geofips, "Data/geofips_adjacency.csv")
save(geofips, file = "Data/geofips_adjacency.RData")

# US County Polygons
us_counties <- counties(class = "sf", year = 2019)
us_counties <- st_transform(us_counties, crs = 4326) # For Intersecting with power plant coordinates
us_countiesEPSG <- st_transform(us_counties, crs = 5070) # Albers Equal Area for accurate area calculations
us_counties$areas <- st_area(us_countiesEPSG) # Calculate area in square meters
us_counties$area_sqmi <- set_units(us_counties$areas, mi^2)

# Turn County Name into County and State
geofips_linking <- geofips %>%
  select("County_Name", "County_GEOID") %>%
  distinct() %>%
  separate("County_Name", into = c("County", "State"), sep = ", ") %>%
  filter(!State %in% c("AS", "PR", "GU", "VI", "MP")) %>%
  mutate(State_Full_Name = state.name[match(State, state.abb)]) %>%
  mutate(State_Full_Name = ifelse(State == "DC", "District of Columbia", State_Full_Name)) %>%
  drop_na()

# Save geofips linking data for later use
write_csv(geofips_linking, "Data/geofips_linking.csv")
save(geofips_linking, file = "Data/geofips_linking.RData")

# Filter data for relevant information
global_power_plant_data_filtered <- global_power_plant_data %>%
  filter(country == "USA") %>%
  select(name, country_long, capacity_mw, primary_fuel, latitude, longitude, commissioning_year)

# Use shapefiles to reverse geocode power plant locations to counties
power_plants_points_sf <- st_as_sf(global_power_plant_data_filtered, coords = c("longitude", "latitude"), crs = 4326)
points_with_county <- st_join(power_plants_points_sf, us_counties, join = st_intersects)

# Combine the reverse geocoded data with the original filtered data and perform necessary cleaning and transformationsss
final_power_plant_data <- points_with_county %>%
  st_drop_geometry() %>%
  filter(!STATEFP %in% c("02", "15", "66", "72")) %>% #Drop AK, HI, and territories
  select(name, country_long, capacity_mw, primary_fuel, commissioning_year, GEOID) %>%
  rename(GeoFIPS = GEOID) %>%
  mutate(commissioning_year = floor(commissioning_year)) %>%
  left_join(geofips_linking, by = c("GeoFIPS" = "County_GEOID")) %>%
  select(name, country_long, capacity_mw, primary_fuel, commissioning_year, GeoFIPS, County, State_Full_Name)

# Save the cleaned data to a new CSV file and RData file
write_csv(final_power_plant_data, "Data/cleaned_power_plant_data.csv")
save(final_power_plant_data, file = "Data/cleaned_power_plant_data.RData")

# Load the full county GDP data
full_county_gdp_data <- read_csv("Data/county_real_gdp_2001_2024.csv")

# Load the full county population data
full_county_population_data <- read_csv("Data/Population Estimates - U.S., States, and Counties.csv", 
                                        col_types = cols(IBRC_Geo_ID = col_character())) %>%
  rename(GeoFIPS = IBRC_Geo_ID) %>%
  mutate(GeoFIPS = str_pad(GeoFIPS, width = 5, side = "left", pad = "0")) # Ensure GeoFIPS is 5 characters with leading zeros

# Load Personal Income Data
full_county_personal_income_data <- read_csv("Data/BEA - US, States, Counties - Per Capita Income.csv",
                                              col_types = cols(IBRC_Geo_ID = col_character())) %>%
  rename(GeoFIPS = IBRC_Geo_ID, PerCapitaIncome = "BEA Per Capita Personal Income") %>%
  mutate(GeoFIPS = str_pad(GeoFIPS, width = 5, side = "left", pad = "0")) # Ensure GeoFIPS is 5 characters with leading zeros

# Clean and reshape the full county GDP data
full_county_gdp_filtered <- full_county_gdp_data %>%
  filter(LineCode == 1) %>% # Filter for real GDP
  pivot_longer(cols = starts_with("20"), names_to = "Year", values_to = "Real_GDP") %>%
  select(GeoFIPS, GeoName, Year, Real_GDP) 

full_county_linked <- full_county_gdp_filtered %>%
  left_join(geofips_linking, by = c("GeoFIPS" = "County_GEOID")) %>%
  select(GeoFIPS, GeoName, Year, Real_GDP, County, State) %>%
  filter((!State %in% c("AK", "HI"))) %>%
  drop_na(County, State) %>%
  mutate(State_Full_Name = state.name[match(State, state.abb)]) %>%
  mutate(State_Full_Name = ifelse(State == "DC", "District of Columbia", State_Full_Name)) %>%
  mutate(Year = as.numeric(Year), Real_GDP = as.numeric(Real_GDP)) %>%
  filter(Year >= 2002 & Year <= 2023)


# Clean and reshape population data
census_years <- c(2010, 2020)
full_population_filtered <- full_county_population_data %>%
  filter(Countyfips != "000", Statefips != "72", Year >= 2002 & Year <= 2023) %>% # Exclude state-level data and Puerto Rico
  filter(!GeoFIPS %in% c("09110", "09120", "09130", "09140", "09150", "09160", "09170", "09180", "09190")) %>% # Exclude CT planning regions
  mutate(preferred = case_when(
    Year %in% census_years & `Count or Estimate` == "Count"   ~ 1L,
    Year %in% census_years                                      ~ 2L,
    !(Year %in% census_years) & `Count or Estimate` == "Estimate" ~ 1L,
    TRUE                                                       ~ 2L
  )) %>%
  group_by(Statefips, Countyfips, Year) %>%  
  slice_min(order_by = preferred, with_ties = FALSE) %>% 
  ungroup() %>% 
  select(-preferred) %>%
  select(!c("Count or Estimate", "State or County Release", "Description"))

# Merge population data and county areas with the linked county GDP data 
full_county_gdp_population_linked <- full_county_linked %>%
  left_join(full_population_filtered, by = c("GeoFIPS", "Year")) %>%
  left_join(us_counties %>% st_drop_geometry() %>% select(GEOID, area_sqmi), by = c("GeoFIPS" = "GEOID")) %>%
  fill(Statefips, Countyfips, Population, .direction = "down") # Fill in population data for years without estimates using the most recent available data for that county

# Calculate GDP Per Capita and Log Values
full_county_gdp_population_linked <- full_county_gdp_population_linked %>%
  mutate(GDP_Per_Capita = Real_GDP / Population,
         lnGDP = log(Real_GDP),
         lnGDP_Per_Capita = log(GDP_Per_Capita),
         area_sqmi = as.numeric(area_sqmi)) %>% # Ensure area is numeric
  mutate(pop_density = Population / area_sqmi) # Calculate population density

# Clean and reshape personal income data
full_personal_income_filtered <- full_county_personal_income_data %>%
  filter(Countyfips != "000", Statefips != "72", Year >= 2002 & Year <= 2023) %>% # Exclude state-level data and Puertoo Rico
  filter(!GeoFIPS %in% c("09110", "09120", "09130", "09140", "09150", "09160", "09170", "09180", "09190")) %>% # Exclude CT planning regions
  select(GeoFIPS, Year, PerCapitaIncome)

# Merge personal income data with the linked county GDP and population data
full_county_gdp_population_income_linked <- full_county_gdp_population_linked %>%
  left_join(full_personal_income_filtered, by = c("GeoFIPS", "Year")) %>%
  fill(PerCapitaIncome, .direction = "down") # Fill in personal income
  
# Get Census Data in 2000 for pre-ACS controls
census2000totalandrace <- get_decennial(geography = "county", 
                            variables = c(totalpop = "P001001",
                                          whitepop = "P006002",
                                          blackpop = "P006003", 
                                          nativepop = "P006004", 
                                          asianpop = "P006005", 
                                          hawaiianpop = "P006006", 
                                          otherpop = "P006007", 
                                          twopluspop = "P006008" 
                                          ),  
                                          
                            year = 2000,
                            sumfile = "sf3",
                            geometry = FALSE) 
census2000educationandpoverty <- get_decennial(geography = "county", 
                            variables = c(poverty = "PCT049002", 
                                          maleassoc = "P037014", 
                                          malebach = "P037015", 
                                          malemasters = "P037016", 
                                          femaleassoc = "P037031", 
                                          femalebach = "P037032", 
                                          femalemasters = "P037033"),  
                            year = 2000, 
                            sumfile = "sf3",
                            geometry = FALSE)

census2000agefarmeremployment <- get_decennial(geography = "county", 
                            variables = c(
                                          malefarmerpop = "P049004",
                                          femalefarmerpop = "P049031",
                                          male6566pop = "P008035",
                                          male6769pop = "P008036",
                                          male7074pop = "P008037",
                                          male7579pop = "P008038",
                                          male8084pop = "P008039",
                                          male85pluspop = "P008040",
                                          fem6566pop = "P008074",
                                          fem6769pop = "P008075",
                                          fem7074pop = "P008076", 
                                          fem7579pop = "P008077",  
                                          fem8084pop = "P008078",  
                                          fem85pluspop = "P008079",
                                          maleunder1pop = "P008003",
                                          male1pop = "P008004",
                                          male2pop = "P008005",
                                          male3pop = "P008006",
                                          male4pop = "P008007",
                                          male5pop = "P008008",
                                          male6pop = "P008009",
                                          male7pop = "P008010",
                                          male8pop = "P008011",
                                          male9pop = "P008012",
                                          male10pop = "P008013",
                                          male11pop = "P008014",
                                          male12pop = "P008015",
                                          male13pop = "P008016",
                                          male14pop = "P008017",
                                          male15pop = "P008018",
                                          male16pop = "P008019",
                                          male17pop = "P008020",
                                          femaleunder1pop = "P008042",
                                          female1pop = "P008043",
                                          female2pop = "P008044",
                                          female3pop = "P008045",
                                          female4pop = "P008046",
                                          female5pop = "P008047",
                                          female6pop = "P008048",
                                          female7pop = "P008049",
                                          female8pop = "P008050",
                                          female9pop = "P008051",
                                          female10pop = "P008052",
                                          female11pop = "P008053",
                                          female12pop = "P008054",
                                          female13pop = "P008055",
                                          female14pop = "P008056",
                                          female15pop = "P008057",
                                          female16pop = "P008058",
                                          female17pop = "P008059",
                                          maleemploy = "P043002",
                                          fememploy = "P043009"),  
                            year = 2000, 
                            sumfile = "sf3",
                            geometry = FALSE)

# Reshape each one and merge together
census2000totalandrace_reshaped <- census2000totalandrace %>%
  pivot_wider(names_from = variable, values_from = value)
census2000educationandpoverty_reshaped <- census2000educationandpoverty %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  select(-NAME)
census2000agefarmeremployment_reshaped <- census2000agefarmeremployment %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  select(-NAME)
census2000_merged <- census2000totalandrace_reshaped %>%
  left_join(census2000educationandpoverty_reshaped, by = "GEOID") %>%
  left_join(census2000agefarmeremployment_reshaped, by = "GEOID") %>%
  rename(GeoFIPS = GEOID)
census2000_merged <- census2000_merged %>%
  filter(!GeoFIPS %in% census2000_merged$GeoFIPS[grepl("^02|^15|^72", census2000_merged$GeoFIPS)]) # Exclude Alaska, Hawaii, and Puerto Rico

rm(list = c("census2000totalandrace", "census2000educationandpoverty", "census2000agefarmeremployment",
            "census2000totalandrace_reshaped", "census2000educationandpoverty_reshaped", "census2000agefarmeremployment_reshaped")) # Clean up intermediate objects to save memory

# Calculate totals
census2000_with_totals <- census2000_merged %>%
         mutate(totalassoc = maleassoc + femaleassoc,
         totalbach = malebach + femalebach,
         totalmasters = malemasters + femalemasters,
         totalelderly = male6566pop + male6769pop+ male7074pop + male7579pop + male8084pop + male85pluspop + 
                        fem6566pop + fem6769pop + fem7074pop + fem7579pop + fem8084pop + fem85pluspop,
         totalunder18 = maleunder1pop + male1pop + male2pop + male3pop + male4pop + male5pop + male6pop + 
                        male7pop + male8pop + male9pop + male10pop + male11pop + male12pop + male13pop + 
                        male14pop + male15pop + male16pop + male17pop + femaleunder1pop + 
                        female1pop + female2pop + female3pop + female4pop + female5pop + female6pop + 
                        female7pop + female8pop + female9pop + female10pop + female11pop + female12pop + 
                        female13pop + female14pop + female15pop + female16pop + female17pop,
         farmerpop = malefarmerpop + femalefarmerpop) %>%
  select(GeoFIPS, totalpop, whitepop, blackpop, nativepop, asianpop, hawaiianpop, otherpop, twopluspop,
         poverty, totalassoc, totalbach, totalmasters, farmerpop, totalelderly, totalunder18, maleemploy, fememploy)

census2000_final <- census2000_with_totals %>%
  mutate(pct_white = whitepop / totalpop,
         pct_black = blackpop / totalpop,
         pct_native = nativepop / totalpop,
         pct_asian = asianpop / totalpop,
         pct_hawaiian = hawaiianpop / totalpop,
         pct_other = otherpop / totalpop,
         pct_twoplus = twopluspop / totalpop,
         pct_poverty = poverty / totalpop,
         pct_assoc = totalassoc / totalpop,
         pct_bach = totalbach / totalpop,
         pct_masters = totalmasters / totalpop,
         pct_farmer = farmerpop / totalpop,
         pct_elderly = totalelderly / totalpop,
         pct_under18 = totalunder18 / totalpop,
         pct_maleemploy = maleemploy / totalpop,
         pct_femaleemploy = fememploy / totalpop,
         Year = 2002) %>%
  select(GeoFIPS, pct_white, pct_black, pct_native, pct_asian, pct_hawaiian, pct_other, pct_twoplus,
         pct_poverty, pct_assoc, pct_bach, pct_masters, pct_farmer, pct_elderly, pct_under18, pct_maleemploy, pct_femaleemploy, Year)

# Get ACS for 2005-2009, 2010 - 2014, and 2015-2019, and 2020-2024 for controls
acs2005_2009 <- get_acs(geography = "county", 
                        variables = c(
                          totalpop = "B01003_001",
                          whitepop = "B02001_002",
                          blackpop = "B02001_003",
                          nativepop = "B02001_004",
                          asianpop = "B02001_005",
                          hawaiianpop = "B02001_006",
                          otherpop = "B02001_007",
                          twopluspop = "B02001_008",
                          poverty = "B17001_002",
                          maleassoc = "B15002_014",
                          femaleassoc = "B15002_031",
                          malebach = "B15002_015",
                          femalebach = "B15002_032",
                          malemasters = "B15002_016",
                          femalemasters = "B15002_033",
                          malefarmerpop = "C24030_003",
                          femalefarmerpop = "C24030_031",
                          male6566pop = "B01001_020",
                          male6769pop = "B01001_021",
                          male7074pop = "B01001_022",
                          male7579pop = "B01001_023",
                          male8084pop = "B01001_024",
                          male85pluspop = "B01001_025",
                          fem6566pop = "B01001_044",
                          fem6769pop = "B01001_045",
                          fem7074pop = "B01001_046",
                          fem7579pop = "B01001_047",
                          fem8084pop = "B01001_048",
                          fem85pluspop = "B01001_049",
                          maleunder5pop = "B01001_003",
                          male59pop = "B01001_004",
                          male1014pop = "B01001_005",
                          male1517pop = "B01001_006",
                          femaleunder5pop = "B01001_027",
                          female59pop = "B01001_028",
                          female1014pop = "B01001_029",
                          female1517pop = "B01001_030",
                          maleemploy = "B23001_002",
                          fememploy = "B23001_088"
                        ), 
                        year = 2009, 
                        geometry = FALSE) %>%
  select(-moe)

acs2010_2014 <- get_acs(geography = "county", 
                        variables = c(
                          totalpop = "B01003_001",
                          whitepop = "B02001_002",
                          blackpop = "B02001_003",
                          nativepop = "B02001_004",
                          asianpop = "B02001_005",
                          hawaiianpop = "B02001_006",
                          otherpop = "B02001_007",
                          twopluspop = "B02001_008",
                          poverty = "B17001_002",
                          maleassoc = "B15002_014",
                          femaleassoc = "B15002_031",
                          malebach = "B15002_015",
                          femalebach = "B15002_032",
                          malemasters = "B15002_016",
                          femalemasters = "B15002_033",
                          malefarmerpop = "C24030_003",
                          femalefarmerpop = "C24030_031",
                          male6566pop = "B01001_020",
                          male6769pop = "B01001_021",
                          male7074pop = "B01001_022",
                          male7579pop = "B01001_023",
                          male8084pop = "B01001_024",
                          male85pluspop = "B01001_025",
                          fem6566pop = "B01001_044",
                          fem6769pop = "B01001_045",
                          fem7074pop = "B01001_046",
                          fem7579pop = "B01001_047",
                          fem8084pop = "B01001_048",
                          fem85pluspop = "B01001_049",
                          maleunder5pop = "B01001_003",
                          male59pop = "B01001_004",
                          male1014pop = "B01001_005",
                          male1517pop = "B01001_006",
                          femaleunder5pop = "B01001_027",
                          female59pop = "B01001_028",
                          female1014pop = "B01001_029",
                          female1517pop = "B01001_030",
                          maleemploy = "B23001_002",
                          fememploy = "B23001_088"
                        ), 
                        year = 2014, 
                        geometry = FALSE) %>%
  select(-moe)

acs2015_2019 <- get_acs(geography = "county", 
                        variables = c(
                          totalpop = "B01003_001",
                          whitepop = "B02001_002",
                          blackpop = "B02001_003",
                          nativepop = "B02001_004",
                          asianpop = "B02001_005",
                          hawaiianpop = "B02001_006",
                          otherpop = "B02001_007",
                          twopluspop = "B02001_008",
                          poverty = "B17001_002",
                          maleassoc = "B15002_014",
                          femaleassoc = "B15002_031",
                          malebach = "B15002_015",
                          femalebach = "B15002_032",
                          malemasters = "B15002_016",
                          femalemasters = "B15002_033",
                          malefarmerpop = "C24030_003",
                          femalefarmerpop = "C24030_031",
                          male6566pop = "B01001_020",
                          male6769pop = "B01001_021",
                          male7074pop = "B01001_022",
                          male7579pop = "B01001_023",
                          male8084pop = "B01001_024",
                          male85pluspop = "B01001_025",
                          fem6566pop = "B01001_044",
                          fem6769pop = "B01001_045",
                          fem7074pop = "B01001_046",
                          fem7579pop = "B01001_047",
                          fem8084pop = "B01001_048",
                          fem85pluspop = "B01001_049",
                          maleunder5pop = "B01001_003",
                          male59pop = "B01001_004",
                          male1014pop = "B01001_005",
                          male1517pop = "B01001_006",
                          femaleunder5pop = "B01001_027",
                          female59pop = "B01001_028",
                          female1014pop = "B01001_029",
                          female1517pop = "B01001_030",
                          maleemploy = "B23001_002",
                          fememploy = "B23001_088"
                        ), 
                        year = 2019, 
                        geometry = FALSE) %>%
  select(-moe)

acs2020_2024 <- get_acs(geography = "county", 
                        variables = c(
                          totalpop = "B01003_001",
                          whitepop = "B02001_002",
                          blackpop = "B02001_003",
                          nativepop = "B02001_004",
                          asianpop = "B02001_005",
                          hawaiianpop = "B02001_006",
                          otherpop = "B02001_007",
                          twopluspop = "B02001_008",
                          poverty = "B17001_002",
                          maleassoc = "B15002_014",
                          femaleassoc = "B15002_031",
                          malebach = "B15002_015",
                          femalebach = "B15002_032",
                          malemasters = "B15002_016",
                          femalemasters = "B15002_033",
                          malefarmerpop = "C24030_003",
                          femalefarmerpop = "C24030_031",
                          male6566pop = "B01001_020",
                          male6769pop = "B01001_021",
                          male7074pop = "B01001_022",
                          male7579pop = "B01001_023",
                          male8084pop = "B01001_024",
                          male85pluspop = "B01001_025",
                          fem6566pop = "B01001_044",
                          fem6769pop = "B01001_045",
                          fem7074pop = "B01001_046",
                          fem7579pop = "B01001_047",
                          fem8084pop = "B01001_048",
                          fem85pluspop = "B01001_049",
                          maleunder5pop = "B01001_003",
                          male59pop = "B01001_004",
                          male1014pop = "B01001_005",
                          male1517pop = "B01001_006",
                          femaleunder5pop = "B01001_027",
                          female59pop = "B01001_028",
                          female1014pop = "B01001_029",
                          female1517pop = "B01001_030",
                          maleemploy = "B23001_002",
                          fememploy = "B23001_088"
                        ), 
                        year = 2024, 
                        geometry = FALSE) %>%
  select(-moe)

# Reshape and calculate percentages for each ACS dataset
acs2005_2009_reshaped <- acs2005_2009 %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  rename(GeoFIPS = GEOID)
acs2010_2014_reshaped <- acs2010_2014 %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  rename(GeoFIPS = GEOID)
acs2015_2019_reshaped <- acs2015_2019 %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  rename(GeoFIPS = GEOID)
acs2020_2024_reshaped <- acs2020_2024 %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  rename(GeoFIPS = GEOID)

acs2005_2009_with_totals <- acs2005_2009_reshaped %>%
  mutate(totalassoc = maleassoc + femaleassoc,
         totalbach = malebach + femalebach,
         totalmasters = malemasters + femalemasters,
        totalelderly = male6566pop + male6769pop+ male7074pop + male7579pop + male8084pop + male85pluspop + 
                       fem6566pop + fem6769pop + fem7074pop + fem7579pop + fem8084pop + fem85pluspop,
                    totalunder18 = maleunder5pop + male59pop + male1014pop + male1517pop + femaleunder5pop + female59pop + female1014pop + female1517pop,
                    farmerpop = malefarmerpop + femalefarmerpop) %>%
  select(GeoFIPS, totalpop, whitepop, blackpop, nativepop, asianpop, hawaiianpop, otherpop, twopluspop,
         poverty, totalassoc, totalbach, totalmasters, farmerpop, totalelderly, totalunder18, maleemploy, fememploy)

acs2005_2009_final <- acs2005_2009_with_totals %>%
  mutate(pct_white = whitepop / totalpop,
         pct_black = blackpop / totalpop,
         pct_native = nativepop / totalpop,
         pct_asian = asianpop / totalpop,
         pct_hawaiian = hawaiianpop / totalpop,
         pct_other = otherpop / totalpop,
         pct_twoplus = twopluspop / totalpop,
         pct_poverty = poverty / totalpop, 
         pct_assoc = totalassoc / totalpop,
         pct_bach = totalbach / totalpop,
         pct_masters = totalmasters / totalpop,
         pct_farmer = farmerpop / totalpop,
         pct_elderly = totalelderly / totalpop,
         pct_under18 = totalunder18 / totalpop,
         pct_maleemploy = maleemploy / totalpop,
         pct_femaleemploy = fememploy / totalpop,
         Year = 2005) %>% 
  select(GeoFIPS, pct_white, pct_black, pct_native, pct_asian, pct_hawaiian, pct_other, pct_twoplus,
         pct_poverty, pct_assoc, pct_bach, pct_masters, pct_farmer, pct_elderly, pct_under18, pct_maleemploy, pct_femaleemploy, Year)

acs2010_2014_with_totals <- acs2010_2014_reshaped %>%
  mutate(totalassoc = maleassoc + femaleassoc,
         totalbach = malebach + femalebach,
         totalmasters = malemasters + femalemasters,
         totalelderly = male6566pop + male6769pop+ male7074pop + male7579pop + male8084pop + male85pluspop + 
                        fem6566pop + fem6769pop + fem7074pop + fem7579pop + fem8084pop + fem85pluspop,
          totalunder18 = maleunder5pop + male59pop + male1014pop + male1517pop + femaleunder5pop + female59pop + female1014pop + female1517pop,
          farmerpop = malefarmerpop + femalefarmerpop) %>%
  select(GeoFIPS, totalpop, whitepop, blackpop, nativepop, asianpop, hawaiianpop, otherpop, twopluspop,
         poverty, totalassoc, totalbach, totalmasters, farmerpop, totalelderly, totalunder18, maleemploy, fememploy)

acs2010_2014_final <- acs2010_2014_with_totals %>%
  mutate(pct_white = whitepop / totalpop,
         pct_black = blackpop / totalpop,
         pct_native = nativepop / totalpop,
         pct_asian = asianpop / totalpop,
         pct_hawaiian = hawaiianpop / totalpop,
         pct_other = otherpop / totalpop,
         pct_twoplus = twopluspop / totalpop,
         pct_poverty = poverty / totalpop, 
         pct_assoc = totalassoc / totalpop,
         pct_bach = totalbach / totalpop,
         pct_masters = totalmasters / totalpop,
         pct_farmer = farmerpop / totalpop,
         pct_elderly = totalelderly / totalpop,
         pct_under18 = totalunder18 / totalpop,
         pct_maleemploy = maleemploy / totalpop,
         pct_femaleemploy = fememploy / totalpop,
         Year = 2010) %>%
  select(GeoFIPS, pct_white, pct_black, pct_native, pct_asian, pct_hawaiian, pct_other, pct_twoplus,
         pct_poverty, pct_assoc, pct_bach, pct_masters, pct_farmer, pct_elderly, pct_under18, pct_maleemploy, pct_femaleemploy, Year)

acs2015_2019_with_totals <- acs2015_2019_reshaped %>%
  mutate(totalassoc = maleassoc + femaleassoc,
         totalbach = malebach + femalebach,
         totalmasters = malemasters + femalemasters,
         totalelderly = male6566pop + male6769pop+ male7074pop + male7579pop + male8084pop + male85pluspop + 
                        fem6566pop + fem6769pop + fem7074pop + fem7579pop + fem8084pop + fem85pluspop,
          totalunder18 = maleunder5pop + male59pop + male1014pop + male1517pop + femaleunder5pop + female59pop + female1014pop + female1517pop,
          farmerpop = malefarmerpop + femalefarmerpop) %>%
  select(GeoFIPS, totalpop, whitepop, blackpop, nativepop, asianpop, hawaiianpop, otherpop, twopluspop,
         poverty, totalassoc, totalbach, totalmasters, farmerpop, totalelderly, totalunder18, maleemploy, fememploy)

acs2015_2019_final <- acs2015_2019_with_totals %>%
  mutate(pct_white = whitepop / totalpop,
         pct_black = blackpop / totalpop,
         pct_native = nativepop / totalpop,
         pct_asian = asianpop / totalpop,
         pct_hawaiian = hawaiianpop / totalpop,
         pct_other = otherpop / totalpop,
         pct_twoplus = twopluspop / totalpop,
         pct_poverty = poverty / totalpop,
         pct_assoc = totalassoc / totalpop,
         pct_bach = totalbach / totalpop,
         pct_masters = totalmasters / totalpop,
         pct_farmer = farmerpop / totalpop,
         pct_elderly = totalelderly / totalpop,
         pct_under18 = totalunder18 / totalpop,
         pct_maleemploy = maleemploy / totalpop,
         pct_femaleemploy = fememploy / totalpop,
         Year = 2015) %>%
  select(GeoFIPS, pct_white, pct_black, pct_native, pct_asian, pct_hawaiian, pct_other, pct_twoplus,
         pct_poverty, pct_assoc, pct_bach, pct_masters, pct_farmer, pct_elderly, pct_under18, pct_maleemploy, pct_femaleemploy, Year)

acs2020_2024_with_totals <- acs2020_2024_reshaped %>%
  mutate(totalassoc = maleassoc + femaleassoc,
         totalbach = malebach + femalebach,
         totalmasters = malemasters + femalemasters,
         totalelderly = male6566pop + male6769pop+ male7074pop + male7579pop + male8084pop + male85pluspop + 
                        fem6566pop + fem6769pop + fem7074pop + fem7579pop + fem8084pop + fem85pluspop,
          totalunder18 = maleunder5pop + male59pop + male1014pop + male1517pop + femaleunder5pop + female59pop + female1014pop + female1517pop,
          farmerpop = malefarmerpop + femalefarmerpop) %>%
  select(GeoFIPS, totalpop, whitepop, blackpop, nativepop, asianpop, hawaiianpop, otherpop, twopluspop,
         poverty, totalassoc, totalbach, totalmasters, farmerpop, totalelderly, totalunder18, maleemploy, fememploy)

acs2020_2024_final <- acs2020_2024_with_totals %>%
  mutate(pct_white = whitepop / totalpop,
         pct_black = blackpop / totalpop,
         pct_native = nativepop / totalpop,
         pct_asian = asianpop / totalpop,
         pct_hawaiian = hawaiianpop / totalpop,
         pct_other = otherpop / totalpop,
         pct_twoplus = twopluspop / totalpop,
         pct_poverty = poverty / totalpop,
         pct_assoc = totalassoc / totalpop,
         pct_bach = totalbach / totalpop,
         pct_masters = totalmasters / totalpop,
         pct_farmer = farmerpop / totalpop,
         pct_elderly = totalelderly / totalpop,
         pct_under18 = totalunder18 / totalpop,
         pct_maleemploy = maleemploy / totalpop,
         pct_femaleemploy = fememploy / totalpop,
         Year = 2020) %>%
  select(GeoFIPS, pct_white, pct_black, pct_native, pct_asian, pct_hawaiian, pct_other, pct_twoplus,
         pct_poverty, pct_assoc, pct_bach, pct_masters, pct_farmer, pct_elderly, pct_under18, pct_maleemploy, pct_femaleemploy, Year)

rm(list = c("acs2005_2009", "acs2010_2014", "acs2015_2019", "acs2020_2024",
            "acs2005_2009_reshaped", "acs2010_2014_reshaped", "acs2015_2019_reshaped", "acs2020_2024_reshaped")) # Clean up intermediate objects to save memory

# Combina All ACS datasets with census
acs_combined <- bind_rows(acs2005_2009_final, acs2010_2014_final, acs2015_2019_final, acs2020_2024_final)

acs_combined <- acs_combined %>%
  filter(!GeoFIPS %in% acs_combined$GeoFIPS[grepl("^02|^15|^72", acs_combined$GeoFIPS)]) # Exclude Alaska, Hawaii, and Puerto Rico

census_controls <- bind_rows(acs_combined, census2000_final)

# Join the census controls with the main county-level panel data
gdp_demo_controls <- full_county_gdp_population_income_linked %>%
  left_join(census_controls, by = c("GeoFIPS", "Year")) %>%
  fill(pct_white, pct_black, pct_native, pct_asian, pct_hawaiian, pct_other, pct_twoplus,
       pct_poverty, pct_assoc, pct_bach, pct_masters, pct_farmer, pct_elderly, pct_under18, pct_maleemploy, pct_femaleemploy, .direction = "down") # Fill in control variables for years without ACS data using the most recent available data for that county

# Load Natural Amenities data and merge with the main dataset
natural_amenities_data <- read_xls("Data/natamenf.xls", skip = 104) %>%
  filter(!STATE %in% c("AK", "HI")) %>% # Exclude Alaska and Hawaii
  select(GeoFIPS = `FIPS Code`, natamen = `Scale`)

gdp_demo_nat <- gdp_demo_controls %>%
  left_join(natural_amenities_data, by = "GeoFIPS")

# Get Urban Areas data to calculate county distance to nearest UA
county_centroids <- us_counties %>%
  filter(!STATEFP %in% c("02", "15", "72", "60", "69", "78")) %>% # Exclude Alaska, Hawaii, and territories
  st_transform(crs = 5070) %>%
  st_centroid()

ua2000 <- get_decennial(geography = "urban area", 
                      variables = c(totalpop = "P001001"), 
                      year = 2000, 
                      sumfile = "sf1",
                      geometry = FALSE) %>%
  rename(GeoFIPS = GEOID) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  select(GeoFIPS, totalpop, NAME)

ua2010 <- get_decennial(geography = "urban area", 
                      variables = c(totalpop = "P001001"), 
                      year = 2010, 
                      sumfile = "sf1",
                      geometry = FALSE) %>%
  rename(GeoFIPS = GEOID) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  select(GeoFIPS, totalpop, NAME)

ua2020 <- get_decennial(geography = "urban area", 
                      variables = c(totalpop = "P1_001N"), 
                      year = 2020, 
                      sumfile = "dhc",
                      geometry = FALSE) %>%
  rename(GeoFIPS = GEOID) %>%
  pivot_wider(names_from = variable, values_from = value) %>%
  select(GeoFIPS, totalpop, NAME)

uashapes2000 <- read_sf("Data/nhgis0001_shapefile_tl2000_us_urb_area_2000/US_urb_area_2000.shp") %>% # Shapefile for 2000 urban areas from NHGIS, unavailable via Tigris
  rename(GeoFIPS = UACODE) %>%
  st_transform(crs = 5070) %>%
  st_centroid()
  
uashapes2012 <- urban_areas(year = 2012) %>%
  rename(GeoFIPS = GEOID10) %>%
  st_transform(crs = 5070) %>%
  st_centroid()

uashapes2020 <- urban_areas(year = 2020) %>%
  rename(GeoFIPS = GEOID10) %>%
  st_transform(crs = 5070) %>%
  st_centroid()

ua2000_25k <- ua2000 %>%
  filter(totalpop >= 25000 & totalpop < 100000) %>%
  mutate(urban_area_category = "25k-100k") %>%
  left_join(uashapes2000 %>% select(GeoFIPS, geometry), by = "GeoFIPS") %>%
  st_as_sf()
ua2000_100k <- ua2000 %>%
  filter(totalpop >= 100000 & totalpop < 250000) %>%
  mutate(urban_area_category = "100k-250k") %>%
  left_join(uashapes2000 %>% select(GeoFIPS, geometry), by = "GeoFIPS") %>%
  st_as_sf()
ua2000_250k <- ua2000 %>%
  filter(totalpop >= 250000 & totalpop < 500000) %>%
  mutate(urban_area_category = "250k-500k") %>%
  left_join(uashapes2000 %>% select(GeoFIPS, geometry), by = "GeoFIPS") %>%
  st_as_sf()
ua2000_500k <- ua2000 %>%
  filter(totalpop >= 500000 & totalpop < 1000000) %>%
  mutate(urban_area_category = "500k-1mil") %>%
  left_join(uashapes2000 %>% select(GeoFIPS, geometry), by = "GeoFIPS") %>% 
  st_as_sf()
ua2000_1mil <- ua2000 %>%
  filter(totalpop >= 1000000) %>%
  mutate(urban_area_category = "1mil+") %>%
  left_join(uashapes2000 %>% select(GeoFIPS, geometry), by = "GeoFIPS") %>%
  st_as_sf()

nearest25kidx2000 <- st_nearest_feature(county_centroids, ua2000_25k)
nearest100kidx2000 <- st_nearest_feature(county_centroids, ua2000_100k)
nearest250kidx2000 <- st_nearest_feature(county_centroids, ua2000_250k)
nearest500kidx2000 <- st_nearest_feature(county_centroids, ua2000_500k)
nearest1milidx2000 <- st_nearest_feature(county_centroids, ua2000_1mil)

us_counties2000 <- us_counties %>%
  filter(!STATEFP %in% c("02", "15", "72", "60", "69", "78"))

us_counties2000$dist_to_urban_25k <- st_distance(
  county_centroids, 
  ua2000_25k[nearest25kidx2000, ], 
  by_element = TRUE
) %>%
  set_units("mi")
us_counties2000$dist_to_urban_100k <- st_distance(
  county_centroids, 
  ua2000_100k[nearest100kidx2000, ], 
  by_element = TRUE
) %>%
  set_units("mi")
us_counties2000$dist_to_urban_250k <- st_distance(
  county_centroids, 
  ua2000_250k[nearest250kidx2000, ], 
  by_element = TRUE
) %>%
  set_units("mi")
us_counties2000$dist_to_urban_500k <- st_distance(
  county_centroids, 
  ua2000_500k[nearest500kidx2000, ], 
  by_element = TRUE
) %>%
  set_units("mi")
us_counties2000$dist_to_urban_1mil <- st_distance(
  county_centroids, 
  ua2000_1mil[nearest1milidx2000, ], 
  by_element = TRUE
) %>%
  set_units("mi")

us_counties2000$Year <- 2002

ua2010_25k <- ua2010 %>%
  filter(totalpop >= 25000 & totalpop < 100000) %>%
  mutate(urban_area_category = "25k-100k") %>%
  left_join(uashapes2012 %>% select(GeoFIPS, geometry), by = "GeoFIPS") %>%
  st_as_sf()
ua2010_100k <- ua2010 %>%
  filter(totalpop >= 100000 & totalpop < 250000) %>%
  mutate(urban_area_category = "100k-250k") %>%
  left_join(uashapes2012 %>% select(GeoFIPS, geometry), by = "GeoFIPS") %>%
  st_as_sf()
ua2010_250k <- ua2010 %>%
  filter(totalpop >= 250000 & totalpop < 500000) %>%
  mutate(urban_area_category = "250k-500k") %>%
  left_join(uashapes2012 %>% select(GeoFIPS, geometry), by = "GeoFIPS") %>%
  st_as_sf()
ua2010_500k <- ua2010 %>%
  filter(totalpop >= 500000 & totalpop < 1000000) %>%
  mutate(urban_area_category = "500k-1mil") %>%
  left_join(uashapes2012 %>% select(GeoFIPS, geometry), by = "GeoFIPS") %>% 
  st_as_sf()
ua2010_1mil <- ua2010 %>%
  filter(totalpop >= 1000000) %>%
  mutate(urban_area_category = "1mil+") %>%
  left_join(uashapes2012 %>% select(GeoFIPS, geometry), by = "GeoFIPS") %>%
  st_as_sf()

nearest25kidx2010 <- st_nearest_feature(county_centroids, ua2010_25k)
nearest100kidx2010 <- st_nearest_feature(county_centroids, ua2010_100k)
nearest250kidx2010 <- st_nearest_feature(county_centroids, ua2010_250k)
nearest500kidx2010 <- st_nearest_feature(county_centroids, ua2010_500k)
nearest1milidx2010 <- st_nearest_feature(county_centroids, ua2010_1mil)

us_counties2010 <- us_counties %>%
  filter(!STATEFP %in% c("02", "15", "72", "60", "69", "78"))

us_counties2010$dist_to_urban_25k <- st_distance(
  county_centroids, 
  ua2010_25k[nearest25kidx2010, ], 
  by_element = TRUE
) %>%
  set_units("mi")
us_counties2010$dist_to_urban_100k <- st_distance(
  county_centroids, 
  ua2010_100k[nearest100kidx2010, ], 
  by_element = TRUE
) %>%
  set_units("mi")
us_counties2010$dist_to_urban_250k <- st_distance(
  county_centroids, 
  ua2010_250k[nearest250kidx2010, ], 
  by_element = TRUE
) %>%
  set_units("mi")
us_counties2010$dist_to_urban_500k <- st_distance(
  county_centroids, 
  ua2010_500k[nearest500kidx2010, ], 
  by_element = TRUE
) %>%
  set_units("mi")
us_counties2010$dist_to_urban_1mil <- st_distance(
  county_centroids, 
  ua2010_1mil[nearest1milidx2010, ], 
  by_element = TRUE
) %>%
  set_units("mi")

us_counties2010$Year <- 2010

ua2020_25k <- ua2020 %>%
  filter(totalpop >= 25000 & totalpop < 100000) %>%
  mutate(urban_area_category = "25k-100k") %>%
  left_join(uashapes2020 %>% select(GeoFIPS, geometry), by = "GeoFIPS") %>%
  st_as_sf()
ua2020_100k <- ua2020 %>%
  filter(totalpop >= 100000 & totalpop < 250000) %>%
  mutate(urban_area_category = "100k-250k") %>%
  left_join(uashapes2020 %>% select(GeoFIPS, geometry), by = "GeoFIPS") %>%
  st_as_sf()
ua2020_250k <- ua2020 %>%
  filter(totalpop >= 250000 & totalpop < 500000) %>%
  mutate(urban_area_category = "250k-500k") %>%
  left_join(uashapes2020 %>% select(GeoFIPS, geometry), by = "GeoFIPS") %>%
  st_as_sf()
ua2020_500k <- ua2020 %>%
  filter(totalpop >= 500000 & totalpop < 1000000) %>%
  mutate(urban_area_category = "500k-1mil") %>%
  left_join(uashapes2020 %>% select(GeoFIPS, geometry), by = "GeoFIPS") %>% 
  st_as_sf()
ua2020_1mil <- ua2020 %>%
  filter(totalpop >= 1000000) %>%
  mutate(urban_area_category = "1mil+") %>%
  left_join(uashapes2020 %>% select(GeoFIPS, geometry), by = "GeoFIPS") %>%
  st_as_sf()

nearest25kidx2020 <- st_nearest_feature(county_centroids, ua2020_25k)
nearest100kidx2020 <- st_nearest_feature(county_centroids, ua2020_100k)
nearest250kidx2020 <- st_nearest_feature(county_centroids, ua2020_250k)
nearest500kidx2020 <- st_nearest_feature(county_centroids, ua2020_500k)
nearest1milidx2020 <- st_nearest_feature(county_centroids, ua2020_1mil)

us_counties2020 <- us_counties %>%
  filter(!STATEFP %in% c("02", "15", "72", "60", "69", "78"))

us_counties2020$dist_to_urban_25k <- st_distance(
  county_centroids, 
  ua2020_25k[nearest25kidx2020, ], 
  by_element = TRUE
) %>%
  set_units("mi")
us_counties2020$dist_to_urban_100k <- st_distance(
  county_centroids, 
  ua2020_100k[nearest100kidx2020, ], 
  by_element = TRUE
) %>%
  set_units("mi")
us_counties2020$dist_to_urban_250k <- st_distance(
  county_centroids, 
  ua2020_250k[nearest250kidx2020, ], 
  by_element = TRUE
) %>%
  set_units("mi")
us_counties2020$dist_to_urban_500k <- st_distance(
  county_centroids, 
  ua2020_500k[nearest500kidx2020, ], 
  by_element = TRUE
) %>%
  set_units("mi")
us_counties2020$dist_to_urban_1mil <- st_distance(
  county_centroids, 
  ua2020_1mil[nearest1milidx2020, ], 
  by_element = TRUE
) %>%
  set_units("mi")

us_counties2020$Year <- 2020

urban_distance_data <- bind_rows(us_counties2000, us_counties2010, us_counties2020) %>%
  rename(GeoFIPS = GEOID) %>%
  select(GeoFIPS, Year, dist_to_urban_25k, dist_to_urban_100k, dist_to_urban_250k, dist_to_urban_500k, dist_to_urban_1mil) %>%
  st_drop_geometry() # Remove geometry to keep only distance data for merging

# Create final panel dataset by merging the urban distance data with the main dataset
final_county_panel <- gdp_demo_nat %>%
  left_join(urban_distance_data, by = c("GeoFIPS", "Year")) %>%
  fill(dist_to_urban_25k, dist_to_urban_100k, dist_to_urban_250k, dist_to_urban_500k, dist_to_urban_1mil, .direction = "down") # Fill in urban distance for years without data using the most recent available data for that county

# Save the final data to a new CSV file and RData file
write_csv(final_county_panel, "Data/cleaned_county_panel.csv")
save(final_county_panel, file = "Data/cleaned_county_panel.RData")

