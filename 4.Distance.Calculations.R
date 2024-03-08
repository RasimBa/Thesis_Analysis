
#.......................Distance Calculations..................................

 
municipality_centroids_upper <- readRDS("municipality_centroids_upper.rds") 
 

# Check for empty geometries
empty_geoms <- st_is_empty(municipality_centroids_upper)

# Filter rows with empty geometries
municipality_with_empty_geoms <- municipality_centroids_upper[empty_geoms, ]

# number of those
num_empty_geoms <- sum(empty_geoms)


house_data <- readRDS("house_data.rds")

# Transform to ETRS89 / UTM zone 32N (EPSG:25832)

house_data <- st_transform(house_data, 25832)

municipality_centroids_upper <- st_transform(municipality_centroids_upper, 25832)

colSums(is.na(municipality_centroids_upper))

house_data <- house_data[!st_is_empty(house_data), ]

 

# Find nearest neighbors
nearest_city <- st_nn(house_data, municipality_centroids_upper, returnDist = TRUE)


 
# Add nearest neighbor ID and distance to house_data_sample
house_data$NearestCityCenterID <- municipality_centroids_upper$ID[unlist(nearest_city$nn)]
house_data$DistanceToNearestCityCenter <- unlist(nearest_city$dist)

 
 
# Convert distance to 1000km by dividing by 1000

house_data$DistanceToNearestCityCenter <- house_data$DistanceToNearestCityCenter / 1000

house_data$InvDistanceToNearestCityCenter <-  1 / house_data$DistanceToNearestCityCenter

 
 
 

####.....Doing spatial analysis in order to find the distance between
#nearest railways, water area, traffic road and so on.

#................... Bahn data ...............................................



Bahnstrecke_sf <- readRDS("Bahnstrecke_sf.rds")

 

Bahnstrecke_sf <- st_transform(Bahnstrecke_sf, crs = 25832)

house_data <- st_transform(house_data, 25832)

house_data <- house_data[!st_is_empty(house_data), ]

Bahnstrecke_sf <- Bahnstrecke_sf[!st_is_empty(Bahnstrecke_sf), ]


#--------------------------------------------------------------------------


railways_buffer_200 <- Bahnstrecke_sf %>% 
  st_buffer(dist = 200)

in_200 <- house_data %>% 
  st_filter(y=railways_buffer_200, .predicate = st_intersects) %>% 
  distinct(uniqueID_gen)  %>% 
  pull()

house_data <- house_data %>%
  mutate(railway_in200 = ifelse(uniqueID_gen %in% in_200, 1, 0))



 

 
 
#--------------------------------------------------------------------------




#-------------------------------------------------------------------------------







#....Reading the data contains info about the water types and their geometry...


# waterbody_file_path <- "/Users/rasimbaghirli/Desktop/THESIS_CODING/dlm250.gk3.shape.ebenen/dlm250_ebenen/gew01_f.shp"

# waterbody_data <- st_read(waterbody_file_path)


# unique(waterbody_data$OBJART_TXT)

# warnings() ##warnings about the usage of decimal points instead of periods

# Recode the values in the "OBJART_TXT" column

# waterbody_data <- waterbody_data %>%
#  mutate(WaterTypes = case_when(
#    OBJART_TXT == "AX_StehendesGewaesser" ~ "Standing Water",
#    OBJART_TXT == "AX_Fliessgewaesser" ~ "Flowing Water",
#    OBJART_TXT == "AX_Meer" ~ "Sea",
#    OBJART_TXT == "AX_Hafenbecken" ~ "Harbour Basin",
#    TRUE ~ OBJART_TXT  # Keep the original value if none of the above conditions match
#  )) %>%
#  select(-OBJART_TXT)  # Remove the original column

# Rename the column
# names(waterbody_data)[names(waterbody_data) == "OBJART_TXT"] <- "WaterTypes"

# saveRDS(waterbody_data, "waterbody_data.rds")




## For this analysis I will create 4 different dataframes with respect to
#WaterTypes in waterbody_data: Then for each dataset I will run st_nn 
#function separately in order to find the nearest distance between houses
#and its nearest watertype. Due to this distance I will create dummy variables
#again for each watertype representing whether is there any e.g standing water,
#sea within 1000-500m(1), 500-0 m(2), or not(0).


waterbody_data <- readRDS("waterbody_data.rds")

##matching waterbody_data's crs to house_data_sample

waterbody_data <- st_transform(waterbody_data, crs = st_crs(house_data))

#creating separate data frames for each water type

StandingWater_sf <- waterbody_data[waterbody_data$WaterTypes == "Standing Water", ]

#saveRDS(StandingWater_sf, "StandingWater_sf.rds")

FlowingWater_sf <- waterbody_data[waterbody_data$WaterTypes == "Flowing Water", ]

#saveRDS(FlowingWater_sf, "FlowingWater_sf.rds")

HarbourBasin_sf <- waterbody_data[waterbody_data$WaterTypes == "Harbour Basin", ]

#saveRDS(HarbourBasin_sf, "HarbourBasin_sf.rds")

Sea_sf <- waterbody_data[waterbody_data$WaterTypes == "Sea", ]

# saveRDS(Sea_sf, "Sea_sf.rds")

#------------------------------------------------------


StandingWater_sf <- readRDS("StandingWater_sf.rds")



StandingWater_sf <- st_transform(StandingWater_sf, crs = 25832)

house_data <- house_data[!st_is_empty(house_data), ]

StandingWater_sf <- StandingWater_sf[!st_is_empty(StandingWater_sf), ]



standingWater_buffer_1000 <- StandingWater_sf %>% 
  st_buffer(dist = 1000)

in_1000_sw <- house_data %>% 
  st_filter(y=standingWater_buffer_1000, .predicate = st_intersects) %>% 
  distinct(uniqueID_gen)  %>% 
  pull()

house_data <- house_data %>%
  mutate(standingWater_in1000 = ifelse(uniqueID_gen %in% in_1000_sw, 1, 0))

 
#------------------------------------------------------


FlowingWater_sf <- readRDS("FlowingWater_sf.rds")



FlowingWater_sf <- st_transform(FlowingWater_sf, crs = 25832)

FlowingWater_sf <- FlowingWater_sf[!st_is_empty(FlowingWater_sf), ]



flowingWater_buffer_1000 <- FlowingWater_sf  %>% 
  st_buffer(dist = 1000)

in_1000_fw <- house_data %>% 
  st_filter(y=flowingWater_buffer_1000, .predicate = st_intersects) %>% 
  distinct(uniqueID_gen)  %>% 
  pull()

house_data <- house_data %>%
  mutate(flowingWater_in1000 = ifelse(uniqueID_gen %in% in_1000_fw, 1, 0))

 
#--------------------------------------------------------

 
 
HarbourBasin_sf <- readRDS("HarbourBasin_sf.rds")



HarbourBasin_sf <- st_transform(HarbourBasin_sf, crs = 25832)

HarbourBasin_sf <- HarbourBasin_sf[!st_is_empty(HarbourBasin_sf), ]


harbourBasin_buffer_1000 <- HarbourBasin_sf  %>% 
  st_buffer(dist = 1000)

in_1000_hb <- house_data %>% 
  st_filter(y=harbourBasin_buffer_1000, .predicate = st_intersects) %>% 
  distinct(uniqueID_gen)  %>% 
  pull()

house_data <- house_data %>%
  mutate(harbourBason_in1000 = ifelse(uniqueID_gen %in% in_1000_hb, 1, 0))



 


 

#----------------------------------------------------------



Sea_sf <- readRDS("Sea_sf.rds")



Sea_sf <- st_transform(Sea_sf, crs = 25832)

Sea_sf <- Sea_sf[!st_is_empty(Sea_sf), ]


sea_buffer_1000 <- Sea_sf  %>% 
  st_buffer(dist = 1000)

in_1000_sea <- house_data %>% 
  st_filter(y=sea_buffer_1000, .predicate = st_intersects) %>% 
  distinct(uniqueID_gen)  %>% 
  pull()

house_data <- house_data %>%
  mutate(lake_in1000 = ifelse(uniqueID_gen %in% in_1000_sea, 1, 0))


 
#------------------------------------------------------------------------------





#...... Reading the dlm data for Agricultural areas..........................

house_data <- st_transform(house_data, 25832) 


agriculturalArea <- readRDS("agriculturalArea.rds")

unique(agriculturalArea$OBJART_TXT)

st_crs(agriculturalArea)

agriculturalArea <- st_transform(agriculturalArea, crs = st_crs(house_data))

agriculturalArea <- agriculturalArea[!st_is_empty(agriculturalArea), ]


agri_buffer_1000 <- agriculturalArea  %>% 
  st_buffer(dist = 1000)

in_1000_agri <- house_data %>% 
  st_filter(y=agri_buffer_1000, .predicate = st_intersects) %>% 
  distinct(uniqueID_gen)  %>% 
  pull()

house_data <- house_data %>%
  mutate(agri_in1000 = ifelse(uniqueID_gen %in% in_1000_agri, 1, 0))

 

 
 
#.........................  Vegetation Area ....................................

#####Looking at the distance between each house and its nearest vegetation area
#The data contain info in the combination of several types of vegetations so called
#Heide, Moor, Sumpf, Unland Vegetationslose Flaeche:In this analysis i will not look at each 
#of those alone.

#'Heide' is a mostly sandy area with typical shrubs, grasses and a 
#small number of trees.


#''Moor' is an uncultivated area whose upper layer consists of
# peat or decomposed plant remains.


#''Swamp' is a waterlogged, intermittently submerged terrain. Spots in the 
#'ground that are temporarily wet after rainfall are not recorded as 'swamps'.

#'Unland/area without vegetation' is an area that is not used for agriculture 
#'on a permanent basis, such as rocky areas that do not protrude from the relief 
#'of the terrain, areas of sand or ice, shore strips along water bodies and 
#succession areas.

#So all above are kind of useless vegetational areas that expected to 
#decrease house price, if a house is close to that.


 

vegetationArea <- readRDS("vegetationArea.rds")

 
 

house_data <- st_transform(house_data, 25832)

st_crs(house_data)

vegetationArea <-  st_transform(vegetationArea, 25832)

st_crs(vegetationArea)

# Remove empty geometries from the house and vegetationArea data
house_data <- house_data[!st_is_empty(house_data), ]

house_data <- st_transform(house_data, 25832)

vegetationArea <- vegetationArea[!st_is_empty(vegetationArea), ]


vegi_buffer_1000 <- vegetationArea  %>% 
  st_buffer(dist = 1000)

 
in_1000_vegi <- house_data %>% 
  st_filter(y=vegi_buffer_1000, .predicate = st_intersects) %>% 
  distinct(uniqueID_gen)  %>% 
  pull()

house_data <- house_data %>%
  mutate(vegi_in1000 = ifelse(uniqueID_gen %in% in_1000_vegi, 1, 0))
 
 

 




#........................... Nearest turbine. ................................



#-------------------------------------------------------------------------------



turbine_final <- turbine_final %>%
  mutate(unit_date.install = dmy(unit_date.install) %>% ymd())


turbine_final <- st_transform(turbine_final, crs = 25832)

turbine_final <- turbine_final[!st_is_empty(turbine_final), ]

house_data <- house_data[!st_is_empty(house_data), ]

# Sort both datasets based on their respective dates
turbine_final <- turbine_final[order(turbine_final$unit_date.install),]

house_data <- house_data[order(house_data$date_finish),]

# Initialize vectors to store the nearest turbine IDs and distances
nearest_turbine_ids <- rep(NA, nrow(house_data))
distances <- rep(NA, nrow(house_data))


# Create a named vector for faster house indexing
house_indices <- setNames(1:nrow(house_data), house_data$house_id)


# Get unique dates from the house dataset
unique_dates <- unique(house_data$date_finish)

 

unique(house_data$date_finish)
for (date in unique_dates) {
  
  message(date)
  
  # Filter turbines installed up to this date
  valid_turbines <- turbine_final[turbine_final$unit_date.install <= date,]
  
  
  # Filter houses sold on this exact date
  potential_houses <- house_data[house_data$date_finish == date,]
  
  # Find the nearest turbine for these houses
  nearest_data <- st_nn(potential_houses, valid_turbines, k = 1, returnDist = TRUE, progress = FALSE)
  
  # Retrieve turbine_ids and distances using matrix form for efficiency
  new_turbine_ids <- valid_turbines$turbine_id[unlist(nearest_data$nn)]
  
  new_distances <- unlist(nearest_data$dist)
  
  
  # Update the results where the distance is either NA (no turbine assigned yet) or the new distance is shorter
  to_update <- is.na(distances[house_indices[potential_houses$house_id]]) | 
    new_distances < distances[house_indices[potential_houses$house_id]]
  
  nearest_turbine_ids[house_indices[potential_houses$house_id][to_update]] <- new_turbine_ids[to_update]
  distances[house_indices[potential_houses$house_id][to_update]] <- new_distances[to_update]
}

# Update the house dataset
house_data$turbine_id <- nearest_turbine_ids
house_data$distance_to_nearest_turbine <- distances

house_data$distance_to_nearest_turbine <- 
  house_data$distance_to_nearest_turbine / 1000


house_data$distance_bands <- cut(house_data$distance_to_nearest_turbine,
                                  breaks = 
                                    c(0, 0.25, 0.5,1, 3, 6, Inf), 
                                  labels = c(" 0-250 m"," 250-500 m"," 500-1000 m",
                                             " 1-3 km", " 3-6 km", " control area"))

# choosing reference group

house_data$distance_bands <- relevel(house_data$distance_bands, 
                                     ref = " control area")


house_data$distance_bands <- as.factor(house_data$distance_bands)
 
 
#------------------------------------------------------------------------------
 

# saveRDS(house_data, "house_Final.rds")
 
 
 