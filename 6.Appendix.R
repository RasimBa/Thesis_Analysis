

house_final_adjusted <- readRDS("house_final_adjusted.rds")
 
 
 
equation <- log(adj_sellprice) ~ turbines_in1km + 
 number_rooms   + bedroom + 
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


model_adjusted <- feols(equation, data = house_final_adjusted, 
                           cluster = "municipality_name")


summary(model_adjusted)


#------------------------------------------------------------------------------




equation1 <- log(adj_sellprice) ~ turbines_in2km + 
  number_rooms   + bedroom + 
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


model_adjusted1 <- feols(equation1, data = house_final_adjusted, 
                        cluster = "municipality_name")


summary(model_adjusted1)



#------------------------------------------------------------------------------


equation2 <- log(adj_sellprice) ~ turbines_in3km + 
  number_rooms   + bedroom + 
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


model_adjusted2 <- feols(equation2, data = house_final_adjusted, 
                        cluster = "municipality_name")


summary(model_adjusted2)



#-------------------------------------------------------------------------------



equation3 <- log(adj_sellprice) ~ turbines_in4km + 
  number_rooms   + bedroom + 
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


model_adjusted3 <- feols(equation3, data = house_final_adjusted, 
                        cluster = "municipality_name")


summary(model_adjusted3)


#-------------------------------------------------------------------------------



equation4 <- log(adj_sellprice) ~ turbines_in5km + 
  number_rooms   + bedroom + 
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


model_adjusted4 <- feols(equation4, data = house_final_adjusted, 
                         cluster = "municipality_name")


summary(model_adjusted4)



#------------------------------------------------------------------------------




equation5 <- log(adj_sellprice) ~ turbines_in6km + 
  number_rooms   + bedroom + 
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


model_adjusted5 <- feols(equation5, data = house_final_adjusted, 
                         cluster = "municipality_name")


summary(model_adjusted5)



#-------------------------------------------------------------------------------


# Tidy up the models' outputs
tidy_adjusted   <- tidy(model_adjusted)   %>% filter(str_detect(term, "turbines_in")) %>% mutate(Model = "(1a)")
tidy_adjusted1  <- tidy(model_adjusted1)  %>% filter(str_detect(term, "turbines_in")) %>% mutate(Model = "(2a)")
tidy_adjusted2  <- tidy(model_adjusted2)  %>% filter(str_detect(term, "turbines_in")) %>% mutate(Model = "(3a)")
tidy_adjusted3  <- tidy(model_adjusted3)  %>% filter(str_detect(term, "turbines_in")) %>% mutate(Model = "(4a)")
tidy_adjusted4  <- tidy(model_adjusted4)  %>% filter(str_detect(term, "turbines_in")) %>% mutate(Model = "(5a)")
tidy_adjusted5  <- tidy(model_adjusted5)  %>% filter(str_detect(term, "turbines_in")) %>% mutate(Model = "(6a)")

# Combine the tidied outputs into one data frame
combined_turbines <- bind_rows(tidy_adjusted, tidy_adjusted1, tidy_adjusted2, 
                               tidy_adjusted3, tidy_adjusted4, tidy_adjusted5)

# Add a column for significance colors and modify term labels
combined_turbines <- combined_turbines %>%
  mutate(significance = case_when(
    p.value < 0.01 ~ "#363004",
    p.value < 0.05 ~ "#736816",
    p.value < 0.1  ~ "#a39317",
    TRUE           ~ "#f5eca6"
  ),
  term = str_replace(term, "turbines_in", "in ")) %>%
  mutate(term = ifelse(str_detect(term, "in1km"), "in 1 km", term),
         term = ifelse(str_detect(term, "in2km"), "in 2 km", term),
         term = ifelse(str_detect(term, "in3km"), "in 3 km", term),
         term = ifelse(str_detect(term, "in4km"), "in 4 km", term),
         term = ifelse(str_detect(term, "in5km"), "in 5 km", term),
         term = ifelse(str_detect(term, "in6km"), "in 6 km", term))

# Ensure data does not contain NA values for estimates
combined_turbines <- na.omit(combined_turbines)

aes(size = abs(estimate))
labs(size = "Magnitude of Effect")

# Figure A2

ggplot(combined_turbines, aes(x = estimate, y = term, color = p.value)) +
  geom_point(aes(size = abs(estimate)), position = position_dodge(width = 0.5), alpha = 0.8) + 
  geom_errorbar(aes(xmin = estimate - std.error, xmax = estimate + std.error), 
                width = 0.2, position = position_dodge(width = 0.5), color = "grey40") +
  facet_grid(. ~ Model) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  labs(y = "Turbine Proximity", x = "Coefficient (Effect Size)", 
       color = "Significance (p-value)", size = "Magnitude of Effect") +
  scale_color_gradient(low="#363004", high="#f5eca6", 
                       breaks = c(0.01, 0.05, 0.1, 1),
                       labels = c("*** p < 0.01", "** p < 0.05", "* p < 0.1", "Not significant")) +
  scale_size_continuous(range = c(3, 6)) + 
  theme_few(base_size = 12, base_family = "sans") + 
  theme(
    legend.position = "bottom",
    legend.box.background = element_rect(colour = "grey90", linewidth = 0.5),
    legend.key.size = unit(1, "cm"),
    legend.spacing.y = unit(0.2, "cm"),   # spacing between legend items
    legend.text = element_text(size = 10),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.direction = "vertical"
  )



#-------------------------------------------------------------------------------


# Convert the sf object to a regular data frame
house_final_adjusted_df <- as.data.frame(house_final_adjusted)


# Reshape the data from wide to long, ensuring correct order

long_data <- house_final_adjusted_df %>%
  select(turbines_in1km, turbines_in2km, turbines_in3km, turbines_in4km, turbines_in5km, turbines_in6km) %>% 
  gather(key = "turbine_distance", value = "treatment") %>%
  mutate(turbine_distance = str_replace_all(turbine_distance, 
                                            c("turbines_in1km" = "in 1km",
                                              "turbines_in2km" = "in 2km",
                                              "turbines_in3km" = "in 3km",
                                              "turbines_in4km" = "in 4km",
                                              "turbines_in5km" = "in 5km",
                                              "turbines_in6km" = "in 6km")))



# Create a stacked bar plot

ggplot(long_data, aes(x = turbine_distance, fill = factor(treatment))) +
  geom_bar(position = "stack") +
  scale_fill_manual(values = c("#a39317", "#363004"), 
                    labels = c("Not Treated", "Treated"), 
                    name = "Treatment Status") +
  labs(x = "Turbine Distance",
       y = "Number of Houses") +
  theme_minimal()




#-------------------------------------------------------------------------------



# ..................... Turbine Heterogeneity ...............................


# Select only desired columns from turbine_final

turbine_final_short <- turbine_final %>% 
  select(turbine_id, hub.height, d_rotor)
 
house_df <- as.data.frame(house_Final)

turbine_df <- as.data.frame(turbine_final_short)

# Merge data frames
house_merged_df <- merge(house_df, turbine_df, by = "turbine_id")

house_merged_df <- subset(house_merged_df, select = -c(geometry.x,geometry.y))

 

class(house_merged_df$hub.height)


house_merged_df$hub.height <- as.numeric(house_merged_df$hub.height)

house_merged_df$d_rotor <- as.numeric(house_merged_df$d_rotor)


house_merged_df <- house_merged_df %>%
  mutate(turbine_category = case_when(
    hub.height >= 0 & hub.height <= 60 ~ "small",
    hub.height > 60 & hub.height <= 90 ~ "medium",
    hub.height > 90 ~ "large",
    TRUE ~ NA_character_
  ))

 

# Filtering the data to keep only unique turbine entries based on turbine ID

unique_turbine_df <- house_merged_df %>%
  distinct(turbine_id, .keep_all = TRUE)


# Figure A3: The Number of Turbines in Turbine Size Category
 

# Create a vector of colors
colors <- c("#635909", "#a39317", "#d1bb11")

# Creating a bar plot to illustrate the number of turbines in each category

barplot(table(unique_turbine_df$turbine_category) / 100,
        main="The Number of The Nearest Wind Turbines in each Category",
        xlab="Turbine Category",
        ylab="Number of Turbines (in hundreds)",
        col=colors,
        ylim=c(0, 50))  

# Define custom labels for the legend

legend_labels <- c("Small Turbines (0-60m)", 
                   "Medium Turbines (60-90m)", "Large Turbines (over 90m)")

# Add a legend with custom labels

legend("topright", legend=legend_labels)


names(house_merged_df)


# saveRDS(house_merged_df, "house_merged_df.rds")

house_merged_df <- readRDS("house_merged_df.rds")

 

# Convert the turbine_category column to a factor
house_merged_df$turbine_category <- as.factor(house_merged_df$turbine_category)

# Now relevel the factor to set "small" as the reference group
house_merged_df$turbine_category <- relevel(house_merged_df$turbine_category, 
                                            ref = "large")


equation <- log(adj_sellprice) ~ turbines_in1km*turbine_category + 
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


model_turbine_hetro <- feols(equation, data = house_merged_df, 
                           cluster = "municipality_name")
 
summary(model_turbine_hetro)



#-------------------------------------------------------------------------------


equation1 <- log(adj_sellprice) ~ turbines_in2km*turbine_category + 
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


model_turbine_hetro1 <- feols(equation1, data = house_merged_df, 
                             cluster = "municipality_name")

summary(model_turbine_hetro1)



#-------------------------------------------------------------------------------


equation2 <- log(adj_sellprice) ~ turbines_in3km*turbine_category + 
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


model_turbine_hetro2 <- feols(equation2, data = house_merged_df, 
                              cluster = "municipality_name")

summary(model_turbine_hetro2)


#-------------------------------------------------------------------------------


equation3 <- log(adj_sellprice) ~ turbines_in4km*turbine_category + 
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


model_turbine_hetro3 <- feols(equation3, data = house_merged_df, 
                              cluster = "municipality_name")

summary(model_turbine_hetro3)




#-------------------------------------------------------------------------------



equation4 <- log(adj_sellprice) ~ turbines_in5km*turbine_category + 
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


model_turbine_hetro4 <- feols(equation4, data = house_merged_df, 
                              cluster = "municipality_name")

summary(model_turbine_hetro4)


#-------------------------------------------------------------------------------

equation5 <- log(adj_sellprice) ~ turbines_in6km*turbine_category + 
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


model_turbine_hetro5 <- feols(equation5, data = house_merged_df, 
                              cluster = "municipality_name")

summary(model_turbine_hetro5)





cn <- c("living_space" = "Living Space", "land_area" = "Land Area",
        "number_floors" = "Number of Floors", "bedroom" = "Number of Bedroom",
        "number_rooms_sqrt" = "Number of Room (squared)", 
        "bathroom" = "Number of Bathroom",
        "bedroom_sqrt" = "Number of Bedroom (squared)",
        "ads_duration" = "Advertisement Duration (in days)",
        "I(cellar)" = "Presence of Cellar",
        "I(parkplace)" = "Presence of Parking Place",
        "I(leased)" = "Leased",
        "I(granny_flat)" = "Presence of Granny Flat",
        "I(guestwc)" = "Presence of Guest WC",
        "number_rooms" = "Number of Rooms",
        "PopulationDensity" = "Population Density",
        "SettlementDensityPerKm" = "Settlement Density",
        "EmploymentDensity_WO" = "Employment Density",
        "WorkplaceCentrality" = "Workplace Centrality",
        "UpperCentreAccessibility" = "Center Accessibility",
        "AirportAccessibility" = "Airport Accessibility",
        "HighwayAccessibility" = "Highway Accessibility",
        "PoliceStations" = "Number of Police Stations",
        "vegi_in1000" = "Vegetation Area in 1km",
        "DistanceToNearestCityCenter" = "Distance to Nearest City Center",
        "lake_in1000" = "Lake in 1 km",
        "agri_in1000" = "Agricultural Area in 1 km",
        "railway_in200"= "Railway Traffic in 200 m",
        "standingWater_in1000" = "Standing Water in 1 km",
        "harbourBason_in1000" = "Harbour Basin in 1 km",
        "flowingWater_in1000" = "Flowing Water in 1 km",
        "distance_bands 0-250 m" = "Distance Band: 0-250 m",
        "distance_bands 250-500 m" = "Distance Band: 250-500 m",
        "distance_bands 500-1000 m" = "Distance Band: 500-1000 m",
        "distance_bands 1-3 km" = "Distance Band: 1-3 km",
        "distance_bands 3-6 km" = "Distance Band: 3-6 km",
        "I(municipality_name)" = "municipality",
        "I(year_quarter)" = "year*quarter",
        "I(year)" = "year",
        " I(state_muniType)" = "state*municipality type",
        "InvDistanceToNearestCityCenter" = "Inv. Distance to City",
        "turbines_in1km" = "Turbines in 1 km",
        "turbines_in2km" = "Turbines in 2 km",
        "turbines_in3km" = "Turbines in 3 km",
        "turbines_in4km" = "Turbines in 4 km",
        "turbines_in5km" = "Turbines in 5 km",
        "turbines_in6km" = "Turbines in 6 km",
        "num_nearby_turbines" = "Turbine Density",
        "turbines_in6km:turbine_categorysmall" = "Turbines in 6km*Small Turbines",
        "turbines_in6km:turbine_categorymedium" = "Turbines in 6km*Medium Turbines",
        "turbines_in6km:turbine_categorylarge" = "Turbines in 6km*Large Turbines",
        "turbines_in5km:turbine_categorysmall" = "Turbines in 5km*Small Turbines",
        "turbines_in5km:turbine_categorymedium" = "Turbines in 5km*Medium Turbines",
        "turbines_in5km:turbine_categorylarge" = "Turbines in 5km*Large Turbines",
        "turbines_in4km:turbine_categorysmall" = "Turbines in 4km*Small Turbines",
        "turbines_in4km:turbine_categorymedium" = "Turbines in 4km*Medium Turbines",
        "turbines_in4km:turbine_categorylarge" = "Turbines in 4km*Large Turbines",
        "turbines_in3km:turbine_categorysmall" = "Turbines in 3km*Small Turbines",
        "turbines_in3km:turbine_categorymedium" = "Turbines in 3km*Medium Turbines",
        "turbines_in3km:turbine_categorylarge" = "Turbines in 3km*Large Turbines",
        "turbines_in2km:turbine_categorysmall" = "Turbines in 2km*Small Turbines",
        "turbines_in2km:turbine_categorymedium" = "Turbines in 2km*Medium Turbines",
        "turbines_in2km:turbine_categorylarge" = "Turbines in 2km*Large Turbines",
        "turbines_in1km:turbine_categorysmall" = "Turbines in 1km*Small Turbines",
        "turbines_in1km:turbine_categorymedium" = "Turbines in 1km*Medium Turbines",
        "turbines_in1km:turbine_categorylarge" = "Turbines in 1km*Large Turbines",
        "turbine_categorysmall" = "Small Turbines",
        "turbine_categorymedium" = "Medium Turbines",
        "turbine_categorylarge" = "Large Turbines")


# Table A1: Regression Results: The Model with Height-Distance Interaction

fc <- modelsummary(list("(1b)" = model_turbine_hetro, 
                        "(2b)" = model_turbine_hetro1, 
                        "(3b)" = model_turbine_hetro2,
                        "(4b)" = model_turbine_hetro3,
                        "(5b)" = model_turbine_hetro4,
                        "(6b)" = model_turbine_hetro5
),
stars = c('.' = .1, '*' = .05, "**" = 0.01, "***" = 0.001),
coef_omit = 5:66,

coef_rename = cn,

notes = c("standard errors in paranthesis"), 

output = "gt",
fmt = 5)   

styled_table <- fc %>%
  gt::tab_header(title = "Results") %>%
  gt::tab_options(table.width = "100%") %>%
  gt::tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_body(
      columns = c("(1b)", "(2b)", "(3b)","(4b)",
                  "(5b)", "(6b)")
    )
  ) %>%
  gt::tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = c("(1b)", "(2b)", "(3b)","(4b)",
                  "(5b)", "(6b)")
    )
  )

gt::gtsave(styled_table, filename = "turbine_hetro.rtf")







 

