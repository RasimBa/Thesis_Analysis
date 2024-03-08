
house_Final <- readRDS("house_Final.rds")

 
 
#------------1.  Spatial Fixed Effects Models    ..............................

# fixed effects: year&state-municipality-type
# clustered errors  by region

#1.1
 
equation_fE <- log(adj_sellprice) ~ number_rooms + bedroom + 
  bathroom + 
  number_floors + land_area + living_space + Age +  I(cellar) +  
  I(parkplace) + I(leased) + I(granny_flat) + I(guestwc) +
  PopulationDensity + Unemployment + AirportAccessibility +
  HighwayAccessibility + PoliceStations + SettlementAndTrafficArea +
  vegi_in1000 + railway_in200 +
  lake_in1000 + DistanceToNearestCityCenter + distance_bands +
  I(condition) + I(furnishing) + I(heating_type) + I(CityType) +
  I(category_house) | I(year) + I(state_muniType)


model_fe <- feols(equation_fE, data = house_Final, cluster = "region")

summary(model_fe)



#-----------------------------------------------------------------------------

# fixed effects: year&municipality
# clustered errors  by region


#1.2

equation_FE1 <- log(adj_sellprice) ~ number_rooms + bedroom + 
  bathroom + number_floors + land_area + living_space + Age + I(cellar) +  
  I(parkplace) + I(leased) + I(granny_flat) + I(guestwc) +
  PopulationDensity + Unemployment + AirportAccessibility +
  HighwayAccessibility + PoliceStations + SettlementAndTrafficArea +
  vegi_in1000 + railway_in200 +
  lake_in1000 + DistanceToNearestCityCenter + distance_bands +
  I(condition) + I(furnishing) + I(heating_type) + I(CityType) +
  I(category_house) | I(year) + I(municipality_name)

 
model_fe1 <- feols(equation_FE1, data = house_Final, cluster = "region")


summary(model_fe1)

 

#-------------------------------------------------------------------------------

# fixed effects: year-quarter & state-municipality-type
# clustered errors  by region


# creating the the year-quarter variable

# Extract year and month
house_Final$year <- format(house_Final$date_finish, "%Y")
house_Final$month <- format(house_Final$date_finish, "%m")

# Determine the quarter based on the month
house_Final$quarter <- ifelse(house_Final$month %in% c("01","02","03"), "Q1", 
                              ifelse(house_Final$month %in% c("04","05","06"), "Q2",
                                     ifelse(house_Final$month %in% c("07","08","09"), "Q3", "Q4")))

# Create the new variable
house_Final$year_quarter <- paste(house_Final$year, house_Final$quarter, sep="_")



#  removing  month, and quarter columns
house_Final <- house_Final[ , !(names(house_Final) %in% c( "month", "quarter"))]

#1.3

equation_FE2 <- log(adj_sellprice) ~ number_rooms   + bedroom + 
  bathroom + 
  number_floors + land_area + living_space + Age +  I(cellar) +  
  I(parkplace) + I(leased) + I(granny_flat) + I(guestwc) +
  PopulationDensity + Unemployment + AirportAccessibility +
  HighwayAccessibility + PoliceStations + SettlementAndTrafficArea +
  vegi_in1000 + railway_in200 +
  lake_in1000 + DistanceToNearestCityCenter + distance_bands +   
  I(condition) + I(furnishing) + I(heating_type) + I(CityType) +
  I(category_house) | I(year_quarter) + I(state_muniType)


model_fe2 <- feols(equation_FE2, data = house_Final, cluster = "region")


summary(model_fe2)


#------------------------------------------------------------------------------

# fixed effects: year&state-municipality 
# clustered errors by municipality

#1.4

equation_FE3 <- log(adj_sellprice) ~ number_rooms  + bedroom + 
  bathroom + 
  number_floors + land_area + living_space + Age +  I(cellar) +  
  I(parkplace) + I(leased) + I(granny_flat) + I(guestwc) +
  PopulationDensity + Unemployment + AirportAccessibility +
  HighwayAccessibility + PoliceStations + SettlementAndTrafficArea +
  vegi_in1000 + railway_in200 +
  lake_in1000 + DistanceToNearestCityCenter + distance_bands +
  I(condition) + I(furnishing) + I(heating_type) + I(CityType) +
  I(category_house) | I(year_quarter) + I(municipality_name)


model_fe3 <- feols(equation_FE3, data = house_Final, cluster = "municipality_name")


summary(model_fe3)


#-------------------------------------------------------------------------------

 


#................2. Spatial Econometric Models   ...............................


#......................2.1 Restricting Data..........................

# Idea here is to subset the original data by keeping as many as observations
# possible in nearer distance bands. The next step is choosing arbitrary cut off
# value of 10 houses in each grid. Thus dropping all grids that contain
# over 10 houses, the spatial balance is tend to be stabilized.

# house_final_adjusted <- readRDS("house_final_adjusted.rds")

# Keep all observations for 0-250m
house_0_250 <- filter(house_Final, distance_bands == " 0-250 m")

# Take 1500 observations from 250-500m
house_250_500 <- filter(house_Final, distance_bands == " 250-500 m") %>%
  sample_n(min(nrow(filter(house_Final, distance_bands == " 250-500 m")), 1600))

# Take 1600 observations from 500-1000m
house_500_1000 <- filter(house_Final, distance_bands == " 500-1000 m") %>%
  sample_n(min(nrow(filter(house_Final, distance_bands == " 500-1000 m")), 1800))

# Take 1700 observations from 1-3km
house_1_3km <- filter(house_Final, distance_bands == " 1-3 km") %>%
  sample_n(min(nrow(filter(house_Final, distance_bands == " 1-3 km")), 1650))

# Take 1800 observations from 3-6km
house_3_6km <- filter(house_Final, distance_bands == " 3-6 km") %>%
  sample_n(min(nrow(filter(house_Final, distance_bands == " 3-6 km")), 1700))

# Take 1950 observations from control
house_control <- filter(house_Final, distance_bands == " control area") %>%
  sample_n(min(nrow(filter(house_Final, distance_bands == " control area")), 1850))

# Create the new dataset by binding rows
house_final_adjusted <- bind_rows(house_0_250, house_250_500, house_500_1000, house_1_3km, house_3_6km, house_control)


# filtering grids with over 10 houses

house_final_adjusted <- house_final_adjusted %>%
  group_by(r1_id) %>%
  mutate(row = row_number()) %>%
  filter(row <= min(10, n())) %>%
  select(-row) %>%
  ungroup()
 

 

 
 
#saveRDS(house_final_adjusted, "house_final_adjusted.rds")


#..............................................................................

 

#...................... 2.2 Weights Matrix ...................................

 

# Extract the data frame component of the sf object

data_frame_component <- st_set_geometry(house_final_adjusted, NULL)
 
# Find the rows with complete cases
complete_cases <- complete.cases(data_frame_component)

# Apply the complete cases to the original sf object
house_final_adjusted <- house_final_adjusted[complete_cases, ]

house_final_adjusted <- st_transform(house_final_adjusted, crs = 25832)

# Check for empty geometries
empty_geoms <- st_is_empty(house_final_adjusted$geometry)

# Subset to remove empty geometries
house_final_adjusted <- house_final_adjusted[!empty_geoms, ]

 # Extract the unique coordinates
coords <- st_coordinates(house_final_adjusted)

# Ensure the coordinates are unique
coords <- coords + matrix(runif(nrow(coords)*2, -0.0000001, 0.0000002), ncol = 2)

# Checking whether there is still duplicated coordinates

num_duplicates <- sum(duplicated(coords) | duplicated(coords, fromLast = TRUE))

print(num_duplicates)

 
# Identify 5 nearest neighbours
knn <- knn2nb(knearneigh(coords, k = 5))

# Create a weights list from the neighbours list
weights_list <- nb2listw(knn, style = "W", zero.policy = TRUE)

# Compute distance matrix
dist_matrix <- as.matrix(dist(coords))

# Compute inverse-distance matrix
# Add a small constant to avoid division by zero
inverse_dist_matrix <- 1 / (dist_matrix + 1e-10)

# Create a binary matrix from the neighbors list
neighbors_matrix <- nb2mat(knn, style = "W", zero.policy = TRUE)

# Multiply the binary neighborhood matrix by the inverse-distance matrix
# This will set the weights of non-neighbors to zero
weights_matrix <- neighbors_matrix * inverse_dist_matrix

# Convert the weights matrix to a list
weights_listw <- mat2listw(weights_matrix, style = "W")

print(weights_listw)

#..............................................................................



#.................... 2.3 Spatial E. Models ...................................

#2.3.1
 

equation_spa <- log(adj_sellprice) ~ number_rooms   + bedroom + 
  bathroom + 
  number_floors + land_area + living_space + Age +  I(cellar) +  
  I(parkplace) + I(leased) + I(granny_flat) + I(guestwc) +
  PopulationDensity + Unemployment + AirportAccessibility +
  HighwayAccessibility + PoliceStations + SettlementAndTrafficArea +
  vegi_in1000 + railway_in200 +
  lake_in1000 + DistanceToNearestCityCenter + distance_bands +
  I(condition) + I(furnishing) + I(heating_type) + I(CityType) +
  I(category_house) + factor(year)


model_lag <- lagsarlm(equation_spa, 
                      data = house_final_adjusted, listw = weights_listw,
                      method = "eigen", zero.policy = TRUE)



summary(model_lag, Nagelkerke = TRUE) 


#..............................................................................


#2.3.2 

model_error <- errorsarlm(equation_spa,
                          data = house_final_adjusted, 
                          listw = weights_listw,
                          method = "eigen", zero.policy = TRUE)


summary(model_error, Nagelkerke = TRUE)

#..............................................................................

#2.3.3

 
model_sarar <-sacsarlm(equation_spa,
                       data = house_final_adjusted, 
                       listw = weights_listw,
                       method = "eigen", zero.policy = TRUE)


summary(model_sarar, Nagelkerke = TRUE)

#..............................................................................


anova(model_error, model_sarar, model_lag)


#...............................................................................


#. Models FE with restricted data that used for spatial econ. models

# 1st Fixed Effect Model with state-municipality-type (higher agg. level), 
# 2nd Fixed Effect Model with municipality (lower agg. level) fixed effects.

# FE Estimation 1

equation_FE1 <- log(adj_sellprice) ~ number_rooms + bedroom + 
  bathroom + number_floors + land_area + living_space + Age + I(cellar) +  
  I(parkplace) + I(leased) + I(granny_flat) + I(guestwc) +
  PopulationDensity + Unemployment + AirportAccessibility +
  HighwayAccessibility + PoliceStations + SettlementAndTrafficArea +
  vegi_in1000 + railway_in200 +
  lake_in1000 + DistanceToNearestCityCenter + distance_bands +
  I(condition) + I(furnishing) + I(heating_type) + I(CityType) +
  I(category_house) | factor(year_quarter) + I(state_muniType)


modelFE_adjusted <- feols(equation_FE1, data = house_final_adjusted, 
                          cluster = "municipality_name")

summary(modelFE_adjusted)

#-----------------------------------------------------------------------------

# FE Estimation 2


equation_FE1_ <- log(adj_sellprice) ~ number_rooms + bedroom + 
  bathroom + number_floors + land_area + living_space + Age + I(cellar) +  
  I(parkplace) + I(leased) + I(granny_flat) + I(guestwc) +
  PopulationDensity + Unemployment + AirportAccessibility +
  HighwayAccessibility + PoliceStations + SettlementAndTrafficArea +
  vegi_in1000 + railway_in200 +
  lake_in1000 + DistanceToNearestCityCenter + distance_bands +
  I(condition) + I(furnishing) + I(heating_type) + I(CityType) +
  I(category_house) | factor(year_quarter) + I(municipality_name)


modelFE_adjusted1 <- feols(equation_FE1_, data = house_final_adjusted, 
                           cluster = "region")

summary(modelFE_adjusted1)





#...............................................................................


#.............3. Robustness Test and Sensitivity Analysis ....................




#...................3.1. Adding additional control variables .................


# harbourBason_in1000,flowingWater_in1000, agri_in1000, standingWater_in1000
# WorkplaceCentrality, ads_duration, turbine_density

 

equation_FE4 <- log(adj_sellprice) ~ number_rooms   + bedroom + 
  bathroom + 
  number_floors + land_area + living_space + Age + ads_duration +
  I(cellar) + I(parkplace) + I(leased) + I(granny_flat) + I(guestwc) +
  PopulationDensity + Unemployment + AirportAccessibility + 
  WorkplaceCentrality + HighwayAccessibility + PoliceStations + 
  SettlementAndTrafficArea +
  vegi_in1000 + harbourBason_in1000 + flowingWater_in1000 + 
  agri_in1000 + standingWater_in1000 + railway_in200 + 
  lake_in1000 + DistanceToNearestCityCenter + distance_bands +
  I(condition) + I(furnishing) + I(heating_type) + I(CityType) +
  I(category_house) | factor(year_quarter) + I(municipality_name)


model_fe4 <- feols(equation_FE4, data = house_Final, 
                   cluster = "municipality_name" )


summary(model_fe4)


#------------------------------------------------------------------------------
  


#.................. 3.2 Calculation of Turbine Density   .......................

 

turbine_final <- st_transform(turbine_final,crs = 25832 )

house_Final <- st_transform(house_Final,crs = 25832 )

# i am trying to find the turbine density , so that for each individual turbine
# i will make a 100m buffer and count the number of turbines in each buffer.
# after finding the nearest turbine for each house, i will also have info that
# how many else turbines are around that turbine. thus by doing so i 
# control for cumulative turbine density effects. 


# Create a 100 meter buffer around each turbine
turbine_buffers <- st_buffer(turbine_final, dist = 100)

# For each buffer, determine which turbines fall within it
turbines_in_buffer <- st_join(turbine_buffers, turbine_final, 
                              join = st_intersects)

# Group by turbine_id   and count the number of turbines in each buffer
#  since a turbine is always in its own buffer,I subtract 1 from the count to exclude it
turbine_density <- turbines_in_buffer %>%
  group_by(turbine_id.x) %>%
  summarise(num_nearby_turbines = n() - 1) 

turbine_density_df <- as.data.frame(turbine_density)

 
turbine_dnsty <- left_join(turbine_final, 
                           turbine_density_df,
                           by = c("turbine_id" = "turbine_id.x"))

 

turbine_dnsty <- subset(turbine_dnsty, select = - geometry.y)

st_geometry(turbine_dnsty) <- "geometry"

house_final_df <- as.data.frame(house_Final)

turbine_dnsty_df <- as.data.frame(turbine_dnsty)

house_Final <- house_final_df %>%
  left_join(turbine_dnsty_df[, c("turbine_id", "num_nearby_turbines")], 
            by = "turbine_id")


# Convert dataframe back to sf object again

house_Final <- st_as_sf(house_Final, geometry = house_Final$geometry, 
                        crs = 25832)

 
 
 
 
 


#-------------------------------------------------------------------------------

# adding turbine density into the regression

equation_FE5 <- log(adj_sellprice) ~ number_rooms   + bedroom + 
  bathroom + 
  number_floors + land_area + living_space + Age + ads_duration +
  I(cellar) + I(parkplace) + I(leased) + I(granny_flat) + I(guestwc) +
  PopulationDensity + Unemployment + AirportAccessibility + 
  WorkplaceCentrality + HighwayAccessibility + PoliceStations + 
  SettlementAndTrafficArea +
  vegi_in1000 + harbourBason_in1000 + flowingWater_in1000 + 
  agri_in1000 + standingWater_in1000 + railway_in200 + 
  lake_in1000 + DistanceToNearestCityCenter + num_nearby_turbines + distance_bands +
  I(condition) + I(furnishing) + I(heating_type) + I(CityType) +
  I(category_house) | factor(year_quarter) + I(municipality_name)


model_fe5 <- feols(equation_FE5, data = house_Final, 
                   cluster = "municipality_name" )


summary(model_fe5)

 
#------------------------------------------------------------------------------


# Instead of using distance bands, I will construct buffers around each turbine,
# and houses within each buffer will get the value of 1, 0, otherwise.

 
 
turbine_final <- st_transform(turbine_final, crs = 25832)

house_Final <- st_transform(house_Final, crs = 25832)

st_crs(turbine_final)

st_crs(house_Final)

 
 
# creating 1km buffer around each turbines, and setting the value of 1 for houses
# within that buffer, 0, otherwise
 

 
turbine_buffer_1km <- turbine_final %>% 
  st_buffer(dist = 1000)

 
turbine_in_1km <- house_Final %>% 
  st_filter(y=turbine_buffer_1km, .predicate = st_intersects) %>% 
  distinct(uniqueID_gen)  %>% 
  pull()

house_Final <- house_Final %>%
  mutate(turbines_in1km = ifelse(uniqueID_gen %in% turbine_in_1km, 1, 0))

 

# creating a buffer of 2km 


turbine_buffer_2km <- turbine_final %>% 
  st_buffer(dist = 2000)


turbine_in_2km <- house_final %>% 
  st_filter(y=turbine_buffer_2km, .predicate = st_intersects) %>% 
  distinct(uniqueID_gen)  %>% 
  pull()

house_Final <- house_Final %>%
  mutate(turbines_in2km = ifelse(uniqueID_gen %in% turbine_in_2km, 1, 0))



# a buffer of 3km

turbine_buffer_3km <- turbine_final %>% 
  st_buffer(dist = 3000)


turbine_in_3km <- house_final %>% 
  st_filter(y=turbine_buffer_3km, .predicate = st_intersects) %>% 
  distinct(uniqueID_gen)  %>% 
  pull()

house_Final <- house_Final %>%
  mutate(turbines_in3km = ifelse(uniqueID_gen %in% turbine_in_3km, 1, 0))


# a buffer or 4km

turbine_buffer_4km <- turbine_final %>% 
  st_buffer(dist = 4000)


turbine_in_4km <- house_final %>% 
  st_filter(y=turbine_buffer_4km, .predicate = st_intersects) %>% 
  distinct(uniqueID_gen)  %>% 
  pull()

house_Final <- house_Final %>%
  mutate(turbines_in4km = ifelse(uniqueID_gen %in% turbine_in_4km, 1, 0))



# a buffer of 5km

turbine_buffer_5km <- turbine_final %>% 
  st_buffer(dist = 5000)


turbine_in_5km <- house_final %>% 
  st_filter(y=turbine_buffer_5km, .predicate = st_intersects) %>% 
  distinct(uniqueID_gen)  %>% 
  pull()

house_Final <- house_Final %>%
  mutate(turbines_in5km = ifelse(uniqueID_gen %in% turbine_in_5km, 1, 0))


# a buffer of 6km

turbine_buffer_6km <- turbine_final %>% 
  st_buffer(dist = 6000)


turbine_in_6km <- house_final %>% 
  st_filter(y=turbine_buffer_6km, .predicate = st_intersects) %>% 
  distinct(uniqueID_gen)  %>% 
  pull()

house_Final <- house_Final %>%
  mutate(turbines_in6km = ifelse(uniqueID_gen %in% turbine_in_6km, 1, 0))


 
 
 
 
#.................. Regression Analysis: Sensitivity ..........................

# Section 8

# Model (1)


equation_robustness <- log(adj_sellprice) ~ turbines_in1km + 
  num_nearby_turbines + number_rooms   + bedroom + 
  bathroom + 
  number_floors + land_area + living_space + Age + ads_duration + I(cellar) +  
  I(parkplace) + I(leased) + I(granny_flat) + I(guestwc) +
  PopulationDensity + Unemployment + AirportAccessibility +
  HighwayAccessibility + PoliceStations + SettlementAndTrafficArea +
  vegi_in1000 + agri_in1000 + railway_in200 +
  lake_in1000 + flowingWater_in1000 + harbourBason_in1000 + standingWater_in1000 +
  DistanceToNearestCityCenter + 
  I(condition) + I(furnishing) + I(heating_type) + I(CityType) +
  I(category_house) | I(year_quarter) + I(municipality_name)


model_robustness <- feols(equation_robustness, data = house_Final, 
                          cluster = "municipality_name")


summary(model_robustness)


#------------------------------------------------------------------------
# Model (2)

equation_robustness1 <- log(adj_sellprice) ~ turbines_in2km + 
  num_nearby_turbines + number_rooms   + bedroom + 
  bathroom + 
  number_floors + land_area + living_space + Age + ads_duration + I(cellar) +  
  I(parkplace) + I(leased) + I(granny_flat) + I(guestwc) +
  PopulationDensity + Unemployment + AirportAccessibility +
  HighwayAccessibility + PoliceStations + SettlementAndTrafficArea +
  vegi_in1000 + agri_in1000 + railway_in200 +
  lake_in1000 + flowingWater_in1000 + harbourBason_in1000 + standingWater_in1000 +
  DistanceToNearestCityCenter +  
  I(condition) + I(furnishing) + I(heating_type) + I(CityType) +
  I(category_house) | I(year_quarter) + I(municipality_name)


model_robustness1 <- feols(equation_robustness1, data = house_Final, 
                          cluster = "municipality_name")


summary(model_robustness1)


#---------------------------------------------------------------------------

# Model (3)

equation_robustness2 <- log(adj_sellprice) ~ turbines_in3km + num_nearby_turbines +
  number_rooms + bedroom + 
  bathroom + 
  number_floors + land_area + living_space + Age + ads_duration + I(cellar) +  
  I(parkplace) + I(leased) + I(granny_flat) + I(guestwc) +
  PopulationDensity + Unemployment + AirportAccessibility +
  HighwayAccessibility + PoliceStations + SettlementAndTrafficArea +
  vegi_in1000 + agri_in1000 + railway_in200 +
  lake_in1000 + flowingWater_in1000 + harbourBason_in1000 + standingWater_in1000 +
  DistanceToNearestCityCenter +
  I(condition) + I(furnishing) + I(heating_type) + I(CityType) +
  I(category_house) | I(year_quarter) + I(municipality_name)


model_robustness2 <- feols(equation_robustness2, data = house_Final, 
                           cluster = "municipality_name")


summary(model_robustness2)

#------------------------------------------------------------------------------

# Model (4)

equation_robustness3 <- log(adj_sellprice) ~ turbines_in4km + 
  num_nearby_turbines + number_rooms   + bedroom + 
  bathroom + 
  number_floors + land_area + living_space + Age + ads_duration + I(cellar) +  
  I(parkplace) + I(leased) + I(granny_flat) + I(guestwc) +
  PopulationDensity + Unemployment + AirportAccessibility +
  HighwayAccessibility + PoliceStations + SettlementAndTrafficArea +
  vegi_in1000 + agri_in1000 + railway_in200 +
  lake_in1000 + flowingWater_in1000 + harbourBason_in1000 + standingWater_in1000 +
  DistanceToNearestCityCenter + 
  I(condition) + I(furnishing) + I(heating_type) + I(CityType) +
  I(category_house) | I(year_quarter) + I(municipality_name)


model_robustness3 <- feols(equation_robustness3, data = house_Final, 
                           cluster = "municipality_name")


summary(model_robustness3)


#------------------------------------------------------------------------------
# Model (5)

equation_robustness4 <- log(adj_sellprice) ~ turbines_in5km + 
  num_nearby_turbines + number_rooms   + bedroom + 
  bathroom + 
  number_floors + land_area + living_space + Age + ads_duration + I(cellar) +  
  I(parkplace) + I(leased) + I(granny_flat) + I(guestwc) +
  PopulationDensity + Unemployment + AirportAccessibility +
  HighwayAccessibility + PoliceStations + SettlementAndTrafficArea +
  vegi_in1000 + agri_in1000 + railway_in200 +
  lake_in1000 + flowingWater_in1000 + harbourBason_in1000 + standingWater_in1000 +
  DistanceToNearestCityCenter + 
  I(condition) + I(furnishing) + I(heating_type) + I(CityType) +
  I(category_house) | I(year_quarter) + I(municipality_name)


model_robustness4 <- feols(equation_robustness4, data = house_Final, 
                           cluster = "municipality_name")


summary(model_robustness4)


#------------------------------------------------------------------------------

# Model (6)

equation_robustness5 <- log(adj_sellprice) ~ turbines_in6km + 
  num_nearby_turbines + number_rooms   + bedroom + 
  bathroom + 
  number_floors + land_area + living_space + Age + ads_duration + I(cellar) +  
  I(parkplace) + I(leased) + I(granny_flat) + I(guestwc) +
  PopulationDensity + Unemployment + AirportAccessibility +
  HighwayAccessibility + PoliceStations + SettlementAndTrafficArea +
  vegi_in1000 + agri_in1000 + railway_in200 +
  lake_in1000 + flowingWater_in1000 + harbourBason_in1000 + standingWater_in1000 +
  DistanceToNearestCityCenter + 
  I(condition) + I(furnishing) + I(heating_type) + I(CityType) +
  I(category_house) | I(year_quarter) + I(municipality_name)


model_robustness5 <- feols(equation_robustness5, data = house_Final, 
                           cluster = "municipality_name")


summary(model_robustness5)




#------------------------------------------------------------------------------


 



 