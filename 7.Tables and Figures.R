# Load the necessary libraries
library(ggplot2)
library(RColorBrewer)
library(tibble)
library(flextable)
library(modelsummary)
library(webshot2)
library(tidyverse)
library(stringr)
library(dplyr)
library(scales)
library(ggpubr)

house_Final <- readRDS("house_Final.rds")

turbine_final <- readRDS("turbine_final.rds")
 
 

 

# Table 1: Descriptive Statistics for Housing Data
 

table1::label(house_Final$sellprice) <- "House Price"
table1::label(house_Final$adj_sellprice) <- "Adjused House Price" 
table1::label(house_Final$number_rooms) <- "Number of Rooms"
table1::label(house_Final$bathroom) <- "Number of Bathrooms"
table1::label(house_Final$number_floors) <- "Number of Floors"
table1::label(house_Final$bedroom) <- "Number of Bedrooms"
table1::label(house_Final$land_area) <- "Land Area (in square meters)"
table1::label(house_Final$living_space) <- "Living Space (in square meters)"
table1::label(house_Final$construction_year) <- "Construction Year"
#table1::label(dataaa_filtered$ads_duration) <- "Duration of Advertisement (in days)"


# Generate the table
table1::table1(~sellprice + number_rooms + adj_sellprice+ bathroom +
                 number_floors + bedroom + land_area +
                 living_space + construction_year   
               , data = house_Final)
# Generate the table with the desired variable order
my_table <- table1::table1(
  ~ sellprice +  adj_sellprice + number_rooms +bathroom +
    number_floors + bedroom + land_area +
    living_space + construction_year,
  data = house_Final
)





#-----------------------------------------------------------------

#  Figure 2: Correlation Table

house_data_copy <- house_Final

# Rename columns
names(house_data_copy)[names(house_data_copy) == "bathroom"] <- "Bathroom"
names(house_data_copy)[names(house_data_copy) == "number_rooms"] <- "Number of Rooms"
names(house_data_copy)[names(house_data_copy) == "bedroom"] <- "Bedroom"
names(house_data_copy)[names(house_data_copy) == "living_space"] <- "Living Space"
names(house_data_copy)[names(house_data_copy) == "land_area"] <- "Land Area"
names(house_data_copy)[names(house_data_copy) == "construction_year"] <- "Year of Construction"

# Now calculate correlation
corr <- cor(dhouse_data_copy[c("Bathroom", "Number of Rooms", 
                                   "Bedroom", "Living Space", "Land Area", 
                                   "Year of Construction")], 
            use = "complete.obs")

# And create your corrplot
corrplot(corr, method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", 
         number.cex = 0.7, 
         order = "hclust", 
         addrect = 2, 
         rect.col = "black", 
         rect.lwd = 2, 
         title = "Correlations Between House Characteristics", 
         mar = c(0,0,1,0)) 






#----------------------------------------------------------------------------




# Summary table for locality characteristics in municipality level


table1::label(house_Final$Population) <- "Total Population Size"
table1::label(house_Final$PopulationDensity) <- "Population Density per km² " 
table1::label(house_Final$Unemployment) <- "Unemployment Rate (in %)"
table1::label(house_Final$HighwayAccessibility) <- "Accessibility to the Nearest Highway in Minute"
table1::label(house_Final$SettlementDensityPerKm) <- "Total Inhabitants per km² Settlement and Trafic Area"
table1::label(house_Final$UpperCentreAccessibility) <- "Accessibility to the Nearest City Center in Minute"
table1::label(house_Final$PublicTransportStops) <- "Total Public Transport Stops"


colSums(is.na(house_Final))


# Generate the table
table1::table1(~ Population+ PopulationDensity + Unemployment + HighwayAccessibility +
                 SettlementDensityPerKm + UpperCentreAccessibility +  PublicTransportStops
               , data = house_Final)



 


#------------------------------------------------------------------------------


# Figure 1: Distribution of House Prices

#Distribution of house selling prices

ggplot(house_Final, aes(x=adj_sellprice)) +  
  geom_histogram(aes(y=after_stat(density)), binwidth=0.1, fill="#363004", 
                 color="black", alpha=0.5) + 
  scale_x_continuous(trans = "log1p", 
                     labels = function(x) scales::comma_format()(x / 1000),
                     breaks = scales::trans_breaks("log1p", function(x) exp(x) - 1),
                     expand = expansion(mult = c(0.01, 0.01))) +
  theme_bw() +
  theme(
    plot.title = element_text(size=13, face="bold", hjust=0.5),
    axis.title.x = element_text(size=16, face="bold"),
    axis.title.y = element_text(size=16, face="bold"),
    axis.text.x = element_text(size=14, angle=45, hjust=1),
    axis.text.y = element_text(size=14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5)
  ) +
  labs(title="Distribution of House Selling Prices (in thousands)",
       x="House Selling Prices (in thousands)", y="Density") + 
  labs(caption = "Note: Data is displayed using a logarithmic scale for better visualization.")




#-----------------------------------------------------------------------------

# Figure 3: Wind Turbine Instalments Across Years


#Filtering turbine data for some visualizations

#number of turbines installed each year  

turbines_by_year <- turbine_final %>%
  mutate(year = year(dmy(unit_date.install))) %>%
  group_by(year) %>%
  summarise(num_turbines = n())

average_count <- mean(turbines_by_year$num_turbines)



##Here I am trying to find the years with the number of installed turbines
#less than 15, and i will not include them into the graph

# Filter years where the number of installed turbines is less than 15
years_less_than_15 <- turbines_by_year %>%
  filter(num_turbines < 15)

# Print the years
print(years_less_than_15$year) ##in the years 1983, 88, 89, 90 there are less than
#15 turbines installed


###geom line

# Filter the data to include only the years starting from 1990
turbines_by_year_filtered <- turbines_by_year %>%
  filter(year >= 1990)

# Find the rows with the max and min number of turbines
max_turbines <- turbines_by_year_filtered[turbines_by_year_filtered$num_turbines == 
                                            max(turbines_by_year_filtered$num_turbines), ]
min_turbines <- turbines_by_year_filtered[turbines_by_year_filtered$num_turbines == 
                                            min(turbines_by_year_filtered$num_turbines), ]

ggplot(turbines_by_year_filtered, aes(x = year, y = num_turbines)) +
  geom_line(color = "grey") +
  geom_point(color = "black") +
  geom_point(data = max_turbines, color = "steelblue") +  
  geom_point(data = min_turbines, color = "red") +  
  geom_hline(yintercept = average_count, color = "orange", linetype = "dashed", size = 0.5) +
  labs(x = "Year", y = "Number of Turbines", title = "The Number of Turbines Installed Each Year in Germany") +
  scale_x_continuous(breaks = seq(1990, max(turbines_by_year_filtered$year), by = 3), limits = c(1990, max(turbines_by_year_filtered$year))) +
  scale_y_continuous(breaks = seq(0, max(turbines_by_year_filtered$num_turbines), by = 100)) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )




#-------------------------------------------------------------------------------


# Figure 4: Turbine Height and Rotor Size
 

colSums(is.na(turbine_final)) #hub.height 495, and d_rotor 281 missing values

na.omit(turbine_final)


 
 

turbine_final$hub.height <- as.numeric(turbine_final$hub.height)
turbine_final$d_rotor <- as.numeric(turbine_final$d_rotor) 




# Plot

# Calculate average hub height and rotor diameter for each year


avg_height_rotor_by_year <- turbine_final %>%
  mutate(year = year(dmy(unit_date.install))) %>%
  group_by(year) %>%
  summarise(average_hub_height = mean(hub.height, na.rm = TRUE),
            average_diameter_rotor = mean(d_rotor, na.rm = TRUE))

# Convert data to long format
avg_height_rotor_by_year_long <- avg_height_rotor_by_year %>%
  pivot_longer(cols = c(average_hub_height, average_diameter_rotor),
               names_to = "measurement",
               values_to = "value")




# Plot data
ggplot(avg_height_rotor_by_year_long, aes(x = year, y = value, color = measurement)) +
  geom_line() +
  labs(x = "Year", y = "Average Value", 
       title = "The Trend in Average Hub Height and Rotor Size over Years") +
  scale_color_manual(name = "Measurement",
                     values = c("average_hub_height" = "#807219", "average_diameter_rotor" = "#38362b"),
                     labels = c("Average Hub Height", "Average Rotor Diameter")) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )



#-------------------------------------------------------------------------------



# Figure 6: Total Number of Wind Turbines by Central Place Categories



# First, we'll save the result of counting the turbines in each category to a new dataframe

turbine_counts <- turbine_final %>%
  group_by(geographic_area_type) %>%
  summarise(n = n(), .groups = "drop")

# Now, we'll create the bar plot

color_map <- c("supplement area" = "#f5eca6", 
               "center" = "#d1bb11", 
               "closer commuter connection area" ="#a39317", 
               "not belonging to metropolitan area" = "#363004", 
               "further commuter integration area" = "#736816")

ggplot(turbine_counts, aes(x = reorder(geographic_area_type, n), y = n, fill = geographic_area_type)) +
  geom_bar(stat = "identity", color = "black") +
  coord_flip() +
  labs(x = "Geographic Area Type", y = "Number of Turbines",
       title = "Number of Turbines in Each Geographic Area Type",
       fill = "Geographic Area Type") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_fill_manual(values = color_map)





#---------------------------------------------------------------------------

# Figure 5: Wind Turbine Locations in Germany by District


# Turbines visualised in Germany(map) -------------------------------------

# Load the district data.
# germany_districts <- st_read("/Users/rasimbaghirli/Desktop/THESIS_CODING/gadm41_DEU_shp/gadm41_DEU_2.shp")

# saveRDS(germany_districts, "germany_districts.rds")

germany_districts <- readRDS("germany_districts.rds")

#View(germany_districts)

# Spatially join the turbines to the districts

turbines_in_districts <- st_join(germany_districts, turbine_final, 
                                 join = st_intersects)

# Count the number of turbines in each district

turbine_counts <- turbines_in_districts %>%
  group_by(NAME_1, NAME_2) %>%
  summarise(turbines = n(), .groups = "drop")

# Convert back to sf object
turbine_counts <- st_as_sf(turbine_counts)

# Replace NA values with 0

turbine_counts$turbines[is.na(turbine_counts$turbines)] <- 0

# Convert wind_turbines_sf to a data frame and add x and y coordinates

wind_turbines_df <- as.data.frame(turbine_final)
wind_turbines_df$x <- st_coordinates(turbine_final)[, 1]
wind_turbines_df$y <- st_coordinates(turbine_final)[, 2]


turbine_map <- ggplot() +
  geom_sf(data = turbine_counts, aes(fill = turbines), color = "#0f0f0f", size = 900) +
  geom_point(data = wind_turbines_df, aes(x = x, y = y), color = "#065934", size = 1.6, shape = 4) +
  scale_fill_gradient(low = "peachpuff", high = "#800e04") +
  theme_minimal() +
  labs(title = "Wind Turbines in Germany by Municipality", fill = "Number of Turbines") +
  theme(
    plot.title = element_text(size = 25, face = "bold"),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white"),
    axis.line = element_blank(), # remove the axis line
    axis.text = element_blank(), # remove axis text
    axis.ticks = element_blank(), # remove axis ticks
    plot.margin = margin(0,0,0,0,"cm") # remove margins
  ) +
  coord_sf(xlim = c(5, 16), ylim = c(47.2, 55))

## Adding image to the plot 

turbine_image <- readPNG("turbine.png", native = TRUE)
 
p_image <- turbine_map +
  inset_element(p=turbine_image, 
                left = 0.01,
                bottom = 0.01,
                right = 0.25,
                top = 0.25)

print(p_image)



# Save the plot

ggsave("turbines.png", plot = p_image, width = 16, 
       height = 16, units = "in")



#----------------------------------------------------------------------------

#------------------------------------------------------------------------------



# Table 2:Descriptive Statistics Turbine Data

 
turbine_final$hub.height <- as.numeric(turbine_final$hub.height)

turbine_final$d_rotor <- as.numeric(turbine_final$d_rotor) 

turbine_final$net_rated_power <- as.numeric(turbine_final$net_rated_power) 

turbine_final <- turbine_final %>%
  mutate(
    year_install = year(dmy(unit_date.install))
  )

# Setting labels for the variables in merged_wind_turbines_sf
table1::label(turbine_final$net_rated_power) <- "Net Power (kW)"

table1::label(turbine_final$hub.height) <- "Hub Height (m)"

table1::label(turbine_final$d_rotor) <- "Rotor Diameter (m)"

table1::label(turbine_final$year_install) <- "Installment Year"

# Generate the table
turbine_table <- table1::table1(
  ~ net_rated_power + hub.height + d_rotor + year_install,
  data = turbine_final
)

print(turbine_table, title = "Descriptive Statistics: Turbine Data")


#-------------------------------------------------------------------------------

# Table 3 Average Wind Turbine Measures by Federal State


#..............Average Wind Turbine Measures by Federal State..................


# Summarise the data
# Generate the summary table
# Convert to a regular dataframe and rename the 'federal_state' column

merged_wind_turbines_df <- as.data.frame(turbine_final)
merged_wind_turbines_df <- rename(merged_wind_turbines_df, 
                                  `Federal State` = federal_state)


merged_wind_turbines_df <- merged_wind_turbines_df %>% drop_na(hub.height, 
                                                               d_rotor, net_rated_power)


merged_wind_turbines_df$hub.height <- as.numeric(merged_wind_turbines_df$hub.height)
merged_wind_turbines_df$d_rotor <- as.numeric(merged_wind_turbines_df$d_rotor) 
merged_wind_turbines_df$net_rated_power <- as.numeric(merged_wind_turbines_df$net_rated_power) 


# Generate the summary table
 merged_wind_turbines_df$plant_date.install <- as.Date(merged_wind_turbines_df$plant_date.install)
class(merged_wind_turbines_df$plant_date.install)


# Generate the summary table
summary_table <- merged_wind_turbines_df %>%
  group_by(`Federal State`) %>%
  summarise(
    `Hub Height (m)` = round(mean(hub.height, na.rm = TRUE), 1),
    `Rotor Diameter (m)` = round(mean(d_rotor, na.rm = TRUE), 1),
    `Rated Power (kW)` = round(mean(net_rated_power, na.rm = TRUE), 1),
    `Total Turbines` = n(),
    .groups = "drop"
  )


# Format the table for display

summary_table %>%
  kable("html") %>%
  kable_styling("striped", full_width = F) %>%
  add_header_above(c(" " = 1, "Average Wind Turbine Measures by Federal State" = 4)) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2:5, bold = F)




#-----------------------------------------------------------------------------

# Figure 8: Percentage of Wind Turbines by Proximity to Houses



#.........HISTOGRAM: percentage of turbines within each distance bands.........

#Calculate the percentage of wind turbines in each proximity category

turbine_counts <- table(house_Final$distance_bands)
turbine_percentages <- prop.table(turbine_counts) * 100

# Create a data frame for the plot
plot_data <- data.frame(proximity = names(turbine_percentages),
                        percentage = as.numeric(turbine_percentages))

# Convert 'proximity' to an ordered factor in order to ordering x axis 

#ascending 

plot_data$proximity <- factor(plot_data$proximity, 
                              levels = c(" 0-250 m"," 250-500 m"," 500-1000 m", 
                                         " 1-3 km", " 3-6 km"," control area"),
                              ordered = TRUE)

 

# Create the bar plot

ggplot(plot_data, aes(x = proximity, y = percentage)) +   
  geom_bar(stat = "identity", color = "black", fill = "#69654b") + 
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.3, size = 3) +
  labs(x = "Proximity to Houses",
       y = "Percentage of Wind Turbines",
       title = "Percentage Distribution of Wind Turbines by Proximity to Houses") +
  theme_light() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 10),
    panel.grid.major = element_blank(),  # Removes major grid lines
    panel.grid.minor = element_blank()   # Removes minor grid lines
  )





 

#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
 

#............................. Box plot  .......................................


# Figure 7: Distribution of House Prices in Each Distance Band

adj_sellprice_thousands <- house_Final$adj_sellprice / 1000

#   house prices and distance bands plus mean value of houses for each band


 

# Calculate mean house price for each distance band
mean_prices <- house_Final %>% 
  group_by(distance_bands) %>% 
  summarise(mean_price = mean(adj_sellprice_thousands, na.rm = TRUE))
 

# Assigning colors

#colors <- c("#f5eca6","#918106", "#a39317", "#736816","#403908","#3d3705")


colors <- c("#e0dbb1", "#998e3f", "#b8a939", "#a89a2c", "#7a701f", "#635909")

adj_sellprice_thousands <- house_Final$adj_sellprice / 1000

# Reorder distance_bands so that "control area" is at the end
house_Final$distance_bands <- factor(house_Final$distance_bands, 
                                     levels = c(" 0-250 m", " 250-500 m", " 500-1000 m", " 1-3 km", " 3-6 km", " control area"))

# Create the box plot

box_plot <- ggplot(house_Final, aes(x = distance_bands, y = adj_sellprice_thousands, fill=distance_bands)) +
  geom_boxplot(notch=FALSE, outlier.shape = NA) + 
  stat_summary(fun = mean, geom = "point", shape = 8, size = 2, color = "red") +  # This adds the mean values
  scale_fill_manual(values = colors) +
  scale_y_continuous("House Prices (in thousands)", 
                     labels = scales::comma, 
                     breaks = seq(50, 500, by = 50), 
                     limits = c(NA, 500)) +
  labs(x = "Distance Bands",
       title = "Distribution of House Prices over Distance Bands") +
  theme_light(base_size = 14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.position = "none",
        axis.title.x = element_text(face="bold"),   # Make x axis title bold
        axis.title.y = element_text(face="bold"),   # Make y axis title bold
        plot.title = element_text(face="bold", size=16, hjust = 0.5),   # Make main title bold
        axis.text.x = element_text(angle = 45, hjust = 1)) +   # Rotate x-axis text for better visibility
  labs(caption = "Note: The black solid line and red star inside each box represent the median (Q2), 
                        and mean value, respectively")



print(box_plot)

means_inbands <- house_Final %>%
  group_by(distance_bands) %>%
  summarise(mean_price = mean(adj_sellprice, na.rm = TRUE))

print(means_inbands)

#-------------------------------------------------------------------------------



house_final_adjusted <- readRDS("house_final_adjusted.rds")

turbine_final <- st_transform(turbine_final, crs = 25832)

house_final <- st_transform(house_final_adjusted, crs = 25832)


turbine_buffer_1km <- turbine_final %>% 
  st_buffer(dist = 1000)


turbine_in_1km <- house_final_adjusted %>% 
  st_filter(y=turbine_buffer_1km, .predicate = st_intersects) %>% 
  distinct(uniqueID_gen)  %>% 
  pull()

house_final_adjusted <- house_final_adjusted %>%
  mutate(turbines_in1km = ifelse(uniqueID_gen %in% turbine_in_1km, 1, 0))


equation <- log(adj_sellprice) ~ number_rooms   + bedroom + 
  bathroom + 
  number_floors + land_area + living_space + Age + ads_duration +
  I(cellar) + I(parkplace) + I(leased) + I(granny_flat) + I(guestwc) +
  PopulationDensity + Unemployment + AirportAccessibility + 
  WorkplaceCentrality + HighwayAccessibility + PoliceStations + 
  SettlementAndTrafficArea +
  vegi_in1000 + harbourBason_in1000 + flowingWater_in1000 + 
  agri_in1000 + standingWater_in1000 + railway_in200 + 
  lake_in1000 + DistanceToNearestCityCenter + turbines_in1km +
  I(condition) + I(furnishing) + I(heating_type) + I(CityType) +
  I(category_house) | factor(year_quarter) + I(municipality_name)


model <- feols(equation, data = house_final_adjusted, 
                   cluster = "municipality_name" )


summary(model)

#-------------------------------------------------------------------------------



#.....................  Regression tables   ...................................

 

 


#------------------------------------------------------------------------------



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
        "num_nearby_turbines" = "Turbine Density")

 
# Table 4: Regression Results of Spatial Fixed Effects Models

ms <- modelsummary(list("FE Model 1" = model_fe, 
                        "FE Model 2" = model_fe1, 
                        "FE Model 3" = model_fe2, 
                        "FE Model 4" = model_fe3),
                   stars = c('.' = .1, '*' = .05, "**" = 0.01, "***" = 0.001),
                   coef_omit = 28:62,  
                   coef_rename = cn,
                   gof_omit = 'AIC|BIC|R2 Within',
                   notes = c("standard errors in paranthesis", 
                             "The interaction term 'state*muniType' represents the combined effect of the state and the type of municipality on the location of properties.",
                             "The interaction term 'year*quarter' captures the nuanced effects specific to each combination of a particular year and quarter.",
                             "year fixed effect variables the year and municipality represent the year a property sold and the muninicipality it locates in, respectively. "),
                   output = "gt",
                   fmt = 5)  # I assume fmt is for formatting, but please ensure it's the correct argument.


styled_table <- ms %>%
  gt::tab_header(title = "Results") %>%
  gt::tab_options(table.width = "100%") %>%
  gt::tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_body(
      columns = c("FE Model 1", "FE Model 2", "FE Model 3", "FE Model 4")
    )
  ) %>%
  gt::tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = c("FE Model 1", "FE Model 2", "FE Model 3", "FE Model 4")
    )
  )

gt::gtsave(styled_table, filename = "fixed_effects.rtf")




# Save table as PNG
if (!requireNamespace("webshot2", quietly = TRUE)) {
  install.packages("webshot2")
}


#-----------------------------------------------------------------------------






#-----------------------------------------------------------------------------

# Table 5: Spatial Econometric Models versus Fixed Effects Estimation

ts <- modelsummary(list("Spatial Lag Model" = model_lag, 
                        "Spatial Error Model" = model_error, 
                        "SARAR/SAC Model" = model_sarar,
                        "FE Estimation 1" = modelFE_adjusted,
                        "FE Estimation 2" = modelFE_adjusted1
                       ),
                   stars = c('.' = .1, '*' = .05, "**" = 0.01, "***" = 0.001),
                 
                   coef_rename = cn,
                  
                   notes = c("standard errors in paranthesis"), 
                             
                   output = "gt",
                   fmt = 5)  # I assume fmt is for formatting, but please ensure it's the correct argument.


styled_table <- ts %>%
  gt::tab_header(title = "Results") %>%
  gt::tab_options(table.width = "100%") %>%
  gt::tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_body(
      columns = c("Spatial Lag Model", "Spatial Error Model", "SARAR/SAC Model",
                  "FE Estimation 1", "FE Estimation 2")
    )
  ) %>%
  gt::tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = c("Spatial Lag Model", "Spatial Error Model", "SARAR/SAC Model",
                  "FE Estimation 1", "FE Estimation 2")
    )
  )

gt::gtsave(styled_table, filename = "sModels_compared.rtf")

 
anova(model_lag,model_error, model_sarar)

#------------------------------------------------------------------------------


# Figure 9: Weights matrix with fixed distance



# Create squares
squares <- st_sfc(st_polygon(list(rbind(c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)))),
                  st_polygon(list(rbind(c(1,0), c(2,0), c(2,1), c(1,1), c(1,0)))))

squares <- st_sf(squares)

# Create circles (buffers) around squares
circles <- st_buffer(st_centroid(squares), dist = 1)  # Increase buffer distance to 1

# Convert to data frames for ggplot
squares_df <- st_coordinates(squares) %>% as.data.frame()
circle1 <- st_cast(circles[1,], "LINESTRING") # First circle
circle2 <- st_cast(circles[2,], "LINESTRING") # Second circle
circles_df1 <- st_coordinates(circle1) %>% as.data.frame() # Circle 1
circles_df2 <- st_coordinates(circle2) %>% as.data.frame() # Circle 2

# Generate random points within squares and circles
set.seed(123)
n_points <- 20
points_in_squares <- st_sample(squares, n_points)
points_in_circles <- st_sample(circles, n_points * 3) %>% st_sf()  # Increase number of points in circles to account for larger area

# Get the points that are in circles but not in squares
points_not_in_squares <- st_difference(points_in_circles, st_union(squares))

# Convert to data frame
points_in_circles_df <- st_coordinates(points_not_in_squares) %>% as.data.frame()

# Convert to data frames for ggplot
points_in_squares_df <- st_coordinates(points_in_squares) %>% as.data.frame()

# Get the coordinates of the centers of the squares
centers <- st_centroid(squares)
centers_df <- data.frame(st_coordinates(centers))
centers_df$label <- c("Grid A", "Grid B")



# Plot
ggplot() +
  geom_polygon(data = squares_df, aes(X, Y), fill = NA, color = "black") +
  geom_path(data = circles_df1, aes(X, Y), color = "#a39317") + # Circle 1
  geom_path(data = circles_df2, aes(X, Y), color = "#403908") + # Circle 2
  geom_point(data = points_in_squares_df, aes(X, Y), color = "black") +
  geom_point(data = points_in_circles_df, aes(X, Y), color = "black") +
  geom_text(data = centers_df, aes(X, Y, label = label), size = 5, color = "darkgreen") + # Add labels
  coord_equal() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank())





#------------------------------------------------------------------------------

# Figure 10: Effects of Wind Turbines on House Prices over Distance Bands


tidy_fe   <- tidy(model_fe)   %>% filter(str_detect(term, "distance_bands"))
tidy_fe1  <- tidy(model_fe1)  %>% filter(str_detect(term, "distance_bands"))
tidy_fe2  <- tidy(model_fe2)  %>% filter(str_detect(term, "distance_bands"))
tidy_fe3  <- tidy(model_fe3)  %>% filter(str_detect(term, "distance_bands"))


coeff_plot <- function(tidy_data, title) {
  ggplot(tidy_data, aes(x = term, y = estimate, ymin = estimate - std.error, ymax = estimate + std.error)) +
    geom_pointrange() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip() +
    labs(title = title, x = NULL, y = "Coefficient") +
    scale_x_discrete(
      labels = c(
        "distance_bands 0-250 m" = "0-250 m",
        "distance_bands 250-500 m" = "250-500 m",
        "distance_bands 500-1000 m" = "500-1000 m",
        "distance_bands 1-3 km" = "1-3 km",
        "distance_bands 3-6 km" = "3-6 km"
      ),
      limits = c(
        "distance_bands 0-250 m",
        "distance_bands 250-500 m",
        "distance_bands 500-1000 m",
        "distance_bands 1-3 km",
        "distance_bands 3-6 km"
      )
    ) +
    theme_minimal()
}

# Arrange and save the plots in one step
ggsave("combined_plot.png", 
       grid.arrange(plot1, plot2, plot3, plot4, ncol = 2),
       width = 10, height = 8, units = "in")

# Generate the plots again with updated names
plot1 <- coeff_plot(tidy_fe, "FE Model 1")
plot2 <- coeff_plot(tidy_fe1, "FE Model 2")
plot3 <- coeff_plot(tidy_fe2, "FE Model 3")
plot4 <- coeff_plot(tidy_fe3, "FE Model 4")

# Then save it
ggsave("combined_plot.png", 
       grid.arrange(plot1, plot2, plot3, plot4, ncol = 2),
       width = 10, height = 8, units = "in")



 


 #----------------------------------------------------------------
 

tidy_fe   <- tidy(model_fe)   %>% filter(str_detect(term, "distance_bands"))
tidy_fe1  <- tidy(model_fe1)  %>% filter(str_detect(term, "distance_bands"))
tidy_fe2  <- tidy(model_fe2)  %>% filter(str_detect(term, "distance_bands"))
tidy_fe3  <- tidy(model_fe3)  %>% filter(str_detect(term, "distance_bands"))

coeff_plot <- function(tidy_data, title) {
  tidy_data <- tidy_data %>%
    mutate(significance = case_when(
      p.value < 0.01 ~ "#363004",
      p.value < 0.05 ~ "#736816",
      p.value < 0.1  ~ "#a39317",
      TRUE           ~ "#f5eca6"
    ))
  
  ggplot(tidy_data, aes(x = term, y = estimate, ymin = estimate - std.error, ymax = estimate + std.error, color = significance)) +
    geom_pointrange() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    coord_flip() +
    labs(title = title, x = NULL, y = "Coefficient") +
    scale_x_discrete(
      labels = c(
        "distance_bands 0-250 m" = "0-250 m",
        "distance_bands 250-500 m" = "250-500 m",
        "distance_bands 500-1000 m" = "500-1000 m",
        "distance_bands 1-3 km" = "1-3 km",
        "distance_bands 3-6 km" = "3-6 km"
      ),
      limits = c(
        "distance_bands 0-250 m",
        "distance_bands 250-500 m",
        "distance_bands 500-1000 m",
        "distance_bands 1-3 km",
        "distance_bands 3-6 km"
      )
    ) +
    scale_color_identity(guide = "legend", 
                         breaks = c("#363004", "#736816", "#a39317", "#f5eca6"),
                         labels = c("*** p < 0.01", "** p < 0.05", "* p < 0.1", "Not significant"),
                         name = "Significance") +
    theme_minimal()
}
 
# Generate the plots
plot1 <- coeff_plot(tidy_fe, "FE Model 1")
plot2 <- coeff_plot(tidy_fe1, "FE Model 2")
plot3 <- coeff_plot(tidy_fe2, "FE Model 3")
plot4 <- coeff_plot(tidy_fe3, "FE Model 4")

# Arrange and save the plots in one step
library(gridExtra)
ggsave("combined_plot.png", 
       grid.arrange(plot1, plot2, plot3, plot4, ncol = 2),
       width = 10, height = 8, units = "in")








#------------------ Results for Robustness Checks:Table -----------------

# Table 6: The Results of Robustness Tests and Sensitivity Analysis

# Get coefficient names
coef_names <- names(coef(model_robustness))

# Get names of coefficients you want to omit (from position 3 onward)
omit_names <- coef_names[3:length(coef_names)]

length(coef_names)


rc <- modelsummary(list("(1)" = model_robustness, 
                        "(2)" = model_robustness1, 
                        "(3)" = model_robustness2,
                        "(4)" = model_robustness3,
                        "(5)" = model_robustness4,
                        "(6)" = model_robustness5
),
stars = c('.' = .1, '*' = .05, "**" = 0.01, "***" = 0.001),
coef_omit = 3:64,

coef_rename = cn,

notes = c("standard errors in paranthesis"), 

output = "gt",
fmt = 5)   

styled_table <- rc %>%
  gt::tab_header(title = "Results") %>%
  gt::tab_options(table.width = "100%") %>%
  gt::tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_body(
      columns = c("(1)", "(2)", "(3)","(4)",
                  "(5)", "(6)")
    )
  ) %>%
  gt::tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      columns = c("(1)", "(2)", "(3)","(4)",
                  "(5)", "(6)")
    )
  )

gt::gtsave(styled_table, filename = "robustnessChecks.rtf")





#----------------------------------------------------------------------------



# Figure 11: Magnitude of Effects for Each Proximity Dummy Variable


 

library(broom)
library(tidyverse)
library(ggplot2)

library(ggthemes) # For additional themes and scales

# Tidy up the models' outputs
tidy_robustness   <- tidy(model_robustness)   %>% filter(str_detect(term, "turbines_in")) %>% mutate(Model = "(1)")
tidy_robustness1  <- tidy(model_robustness1)  %>% filter(str_detect(term, "turbines_in")) %>% mutate(Model = "(2)")
tidy_robustness2  <- tidy(model_robustness2)  %>% filter(str_detect(term, "turbines_in")) %>% mutate(Model = "(3)")
tidy_robustness3  <- tidy(model_robustness3)  %>% filter(str_detect(term, "turbines_in")) %>% mutate(Model = "(4)")
tidy_robustness4  <- tidy(model_robustness4)  %>% filter(str_detect(term, "turbines_in")) %>% mutate(Model = "(5)")
tidy_robustness5  <- tidy(model_robustness5)  %>% filter(str_detect(term, "turbines_in")) %>% mutate(Model = "(6)")

# Combine the tidied outputs into one data frame
combined_turbines <- bind_rows(tidy_robustness, tidy_robustness1, tidy_robustness2, 
                               tidy_robustness3, tidy_robustness4, tidy_robustness5)

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
  scale_size_continuous(range = c(2, 6)) + 
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





#------------------------------------------------------------------------------

# Figure 12: Number of Properties in Each Proximity Category Based on Treatment Status




# Convert the sf object to a regular data frame
house_final_df <- as.data.frame(house_Final)


# Reshape the data from wide to long, ensuring correct order
long_data <- house_final_df %>%
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
 
#.............................. Figure 4A ......................................

# Calculate the percentage for each category without adding the variable to house_final

distance_percentage <- house_Final %>%
  mutate(distance_category = cut(distance_to_nearest_turbine, 
                                 breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, Inf), 
                                 labels = c("1km", "2km", "3km", "4km", "5km", "6km", "7km", "8km", "9km", "10km","Further Away"),
                                 include.lowest = TRUE, right = FALSE)) %>%
  group_by(distance_category) %>% 
  summarise(count = n()) %>% 
  mutate(percentage = count / sum(count) * 100)


# Load necessary library
library(scales)

# Order the shades based on the percentage
ordered_shades <- scales::alpha("#7a701f", seq(0.4, 1, length.out = nrow(distance_percentage)))

# Create a named vector of shades, with names being the distance categories
color_shades <- setNames(ordered_shades, distance_percentage$distance_category[order(distance_percentage$percentage)])

# Plot the bar graph with shades of color based on percentage and other modifications
ggplot(distance_percentage, aes(x = distance_category, y = percentage, fill = distance_category)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(y = "Percentage", x = "Distance to Nearest Turbine", title = "Percentage of Turbines by Distance") +
  theme_minimal() +
  scale_fill_manual(values = color_shades, name = "Distance Category") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme(
    legend.position = "right",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold")
  )




#------------------------------------------------------------------------------
names(house_Final)




 





 