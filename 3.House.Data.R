
readRDS("house_data_raw.rds") -> dataaa # i will need the row house data for some
#calculations

# Select rows with duplicated Unique_ID
duplicated_rows <- dataaa[duplicated(dataaa$uniqueID_gen) | 
                            duplicated(dataaa$uniqueID_gen, fromLast = TRUE), ]

# Print the duplicated rows
print(duplicated_rows)
colSums(is.na(dataaa))

dataaa$date_start <- paste(dataaa$ajahr, dataaa$amonat, "01", sep = "-")

# Convert to date type
dataaa$date_start <- as.Date(dataaa$date_start)
print(dataaa$date_start)
print(dataaa["date_start"])


# Merge month and year columns - date when advertisement is deleted 
dataaa$date_finish <- paste(dataaa$ejahr, dataaa$emonat, "01", sep = "-")
dataaa$date_finish <- as.Date(dataaa$date_finish)
print(dataaa["date_finish"])





# Select columns with more than 5ml NAs ----------------------------------


selected_cols <- colSums(is.na(dataaa)) > 5000000
selected_cols
dataaa[,selected_cols]

colSums(is.na(dataaa))


# delete columns containing over 5ml NAs
dataaa <- subset(dataaa, select = - selected_cols)



dataaa <- subset(dataaa, select = -c(energieeffizienzklasse, letzte_modernisierung, 
                                     mieteinnahmenpromonat, bauphase, nebenraeume, 
                                     ev_kennwert, nutzflaeche, rollstuhlgerecht, 
                                     ev_wwenthalten, energieausweistyp, 
                                     ajahr, amonat, ejahr, emonat, 
                                     flag_aufzug, flag_gaestewc, 
                                     flag_einliegerwohnung, flag_baujahr, 
                                     flag_denkmalobjekt, flag_kaufvermietet, 
                                     flag_keller, denkmalobjekt, 
                                     ferienhaus, parkplatzpreis))

dataaa <- subset(dataaa, select = -c(flag_ferienhaus, flag_parkplatz))


#NAs and deal with NAs  -------------------------------------------------


colSums(is.na(dataaa))
names(dataaa)


#dataaa <- dataaa[complete.cases(dataaa$anzahletagen, 
#dataaa$wohnflaeche, dataaa$r1_id, 
#dataaa$schlafzimmer, dataaa$kaufpreis, 
#dataaa$zimmeranzahl, dataaa$grundstuecksflaeche), 

#dataaa$ausstattung, dataaa$kaufvermietet, dataaa$einliegerwohnung, dataaa$]





dataaa$objektzustand <- factor(dataaa$objektzustand)

dataaa$objektzustand <- droplevels(dataaa$objektzustand, 
                                   exclude = "Dilapidated")

#adding house id column 
dataaa$house_id <- paste0("house_id", seq_along(dataaa$r1_id))

# Filter for house prices between 60,000 and 2,500,000
dataaa_filtered <- subset(dataaa, 
                          kaufpreis >= 60000 & kaufpreis <= 2500000 &
                            zimmeranzahl >= 1 & zimmeranzahl <= 20 &
                            wohnflaeche >= 40 & wohnflaeche <= 800 &
                            grundstuecksflaeche >= 20 & grundstuecksflaeche <=
                            6000 &
                            kategorie_Haus %in% c("Single-family house", "Bungalow",
                                                  "Mansion","Semi-detached house",
                                                  "Farmhouse","Terraced house") &
                            baujahr > 1950 &
                            schlafzimmer > 0 & schlafzimmer <= 8 &
                            anzahletagen <= 5)



dataaa_filtered <- dplyr::rename(dataaa_filtered, 
                                 bedroom = schlafzimmer,
                                 postcode = plz,
                                 sellprice = kaufpreis,
                                 parkplace = parkplatz,
                                 construction_year = baujahr,
                                 condition = objektzustand,
                                 granny_flat = einliegerwohnung,
                                 cellar = keller,
                                 land_area = grundstuecksflaeche,
                                 living_space = wohnflaeche,
                                 number_floors = anzahletagen,
                                 number_rooms = zimmeranzahl,
                                 category_house = kategorie_Haus,
                                 bathroom=badezimmer,
                                 elevator=aufzug,
                                 heating_type=heizungsart,
                                 furnishing = ausstattung,
                                 leased = kaufvermietet,
                                 region = blid, 
                                 guestwc = gaestewc)

 
colSums(is.na(dataaa_filtered))

##replacing NA values with unknown 

dataaa_filtered$heating_type[is.na(dataaa_filtered$heating_type)] <-
  "unknown_heating"

dataaa_filtered$furnishing[is.na(dataaa_filtered$furnishing)] <- 
  "unknown_furnishing"

dataaa_filtered$erg_amd[is.na(dataaa_filtered$erg_amd)] <- 
  "unknown_erg_amd"


dataaa_filtered$postcode[is.na(dataaa_filtered$postcode)] <- 
  "unknown_postcode"

#-----------------------------------------------------------------------
 

#-----------------------------------------------------------------
#generating unique house_id identifiers for each house

dataaa_filtered$house_id <- paste0("house_id", seq_along(dataaa_filtered$r1_id))

 

names(dataaa_filtered)



#....................downloading inflation data of germany 2009-2020...........



# inf <- read.csv("inflation_2009_2020.csv",header = FALSE, sep = ';')

# saveRDS(inf, "inflation.rds")


inf <- readRDS("inflation.rds")

View(inf)

# converting columns into numeric and replacing - character with NA

na.omit(inf)
 
 
inf <- inf %>%
   mutate(
     V3 = replace(V3, V3 == "-", NA),
     V4 = replace(V4, V4 == "-", NA),
     V5 = replace(V5, V5 == "-", NA)
   ) %>%
   mutate(
     V3 = as.numeric(V3),
     V4 = as.numeric(V4),
     V5 = as.numeric(V5)
   )



# Delete the first six rows
inf <- inf[-c(1:6, 151:154), ]

# Reset the row names
rownames(inf) <- NULL

names(inf) <- c("years", "months", "cons_pr_index", 
                "change_on_previous_years_month", 
                "change_on_previous_month")


# Replace "-" with NA in the "change_on_previous_month" column

inf$change_on_previous_month <- ifelse(inf$change_on_previous_month ==
                                         "-", NA, 
                                       inf$change_on_previous_month)


# Define a vector of month names and corresponding numeric values
month_names <- c("January", "February", "March", "April", 
                 "May", "June", "July", "August", "September", 
                 "October", "November", "December")

# create a vector of 2-digit numbers (01, 02, ..., 12)
month_numbers <- sprintf("%02d", 1:12) 

# Use match() function to convert month names to numbers
inf$months <- month_numbers[match(inf$months, month_names)]

# Merge months and years columns 
inf$date <- paste(inf$years, inf$months, "01", sep = "-")

# Convert to date type
inf$date <- as.Date(inf$date)
print(inf$date)
print(inf["date"])

na.omit(inf)

# deleting years and months column
inf <- subset(inf, select = -c(years, months))

## to shift the date column to the from
inf <- inf %>% select(date, everything())


# Merge the two data frames based on the date columns

dataaa_filtered <- inner_join(dataaa_filtered, inf, by = c("date_finish" = "date"))


#..................adjusting house prices for inflation..................

 
dataaa_filtered$adj_sellprice <- dataaa_filtered$sellprice / 
  (dataaa_filtered$cons_pr_index / 100)


#creating advertisement duration column
dataaa_filtered$ads_duration <- dataaa_filtered$date_finish - dataaa_filtered$date_start


class(dataaa_filtered$ads_duration) 

dataaa_filtered$ads_duration <- as.numeric(dataaa_filtered$ads_duration)



#--------------------------------------------------------------------------



#.....Merging House Data with Inkar, House Geocoordinates and other datasets.


#reading the dataframe that contains municipality ID codes and names of the houses

house_munici_codes <- readRDS("municipalities.rds")  

 

#.........Merging the data with house municipality codes and names with the 
# row house data
 

dataaa_munici_codes <- merge(dataaa_filtered, house_munici_codes,
                             by = "uniqueID_gen")



colSums(is.na(dataaa_munici_codes))

#merging the same data that contains house municipality codes and 
# names with the filtered house data

dataaa_filtered_municipality <- merge(dataaa_filtered, house_munici_codes, 
                                      by = "uniqueID_gen", all.x = TRUE)


colSums(is.na(dataaa_filtered_municipality)) #there are 405497 houses that 
#house_municipality data frame has no info about that


# Read the Excel file that holds municipality reference codes



excel_reference <- read_excel("Gemeindereferenzen.xlsx")

# Replace scientific notation with regular numbers in gem19 and gem19rs columns
excel_reference$gem19 <- format(excel_reference$gem19, scientific = FALSE)
excel_reference$gem19rs <- format(excel_reference$gem19rs, scientific = FALSE)




# Convert all columns to character
excel_reference <- as.data.frame(lapply(excel_reference, as.character))

# Check the updated class of the columns
print(sapply(excel_reference, class))

# Convert the gem19_0 column to a character type
excel_reference$gem19_0 <- as.character(excel_reference$gem19_0)
excel_reference$gem19 <- as.character(excel_reference$gem19)
excel_reference$gem19rs <- as.character(excel_reference$gem19rs)
excel_reference$name19 <- as.character(excel_reference$name19)

# Write the data frame to a CSV file
write.csv(excel_reference, "Gemeindereferenzen.csv", row.names = FALSE)

# Read the CSV file, colClasses = c("gem19_0" = "character") interrupts r 
#to treat the column  gem19_0 as numeric which deletes 0s in the beginning 
munici_reference <- read.csv("Gemeindereferenzen.csv", 
                             colClasses = c("gem19_0" = "character",
                                            "gem19rs" = "character",
                                            "gem19" = "character",
                                            "name19" = "character"))




#now the filtered house data is getting merged with the municipality 
#reference data that contains different versions of the municipality code
#this's important to merge the house data with inkar data, because inkar data
# contains gem19 style municipality codes, that house data has gem19_0.



dataaa_filtered_munici_codes <- dataaa_filtered_municipality %>%
  left_join(munici_reference[c("gem19_0", "gem19rs", "gem19")], 
            by = c("AGS_gem0" = "gem19_0"))







#.......Here reading the INKAR data that contains socio-demographic variables

# socio_demo_data <- read.csv("socio_demo_data.csv", 
#                            header = TRUE, sep = ';')

# saveRDS(socio_demo_data , "socio_demo_data.rds")


socio_demo_data <- readRDS("socio_demo_data.rds")


#since the 1s rows show year of the last available data for a variable, we 
#delete the 1st rows.


socio_demo_data <- socio_demo_data[-1,]

rownames(socio_demo_data) <- NULL # i want the rows start from 1 again

#counting empty rows
empty_rows <- sapply(socio_demo_data, function(x) sum(x == ""))

# Print the counts
print(empty_rows) #Einwohner.je.Arzt=5095, Kinosäle=31
#Schüler.an.allgemeinbildenden.Schulen.je.1.000.Einwohner=205. I have to replace
#them with NA


####........Changing column names

new_names <- c(
  "CentralLocationStatus" = "Zentralörtlicher.Status..zusammengefasst.",
  "CityType" = "Stadt..Gemeindetyp",
  "Population" = "Bevölkerung",
  "PopulationDensity" = "Einwohnerdichte",
  "SettlementAndTrafficArea" = "Siedlungs..und.Verkehrsfläche",
  "SettlementDensityPerKm" = "Siedlungsdichte.in.km.",
  "PoliceStations" = "Polizeidienststellen",
  "EmploymentDensity_AO" = "Beschäftigtendichte..AO.",
  "EmploymentDensity_WO" = "Beschäftigtendichte..WO.",
  "WorkplaceCentrality" = "Arbeitsplatzzentralität",
  "Unemployment" = "Arbeitslosigkeit",
  "TrainStations" = "Bahnhaltestellen",
  "BusStops" = "Bushaltestellen",
  "HighwayAccessibility" = "Erreichbarkeit.Autobahnen",
  "AirportAccessibility" = "Erreichbarkeit.Flughäfen",
  "UpperCentreAccessibility" = "Erreichbarkeit.von.Oberzentren",
  "PublicTransportStops" = "ÖV.Haltestellen",
  "ResidentsPerDoctor" = "Einwohner.je.Arzt",
  "StudentsPer1000Residents" = "Schüler.an.allgemeinbildenden.Schulen.je.1.000.Einwohner",
  "gem_19" = "Kennziffer",
  "municipality_name" = "Raumeinheit"
)

# Renaming the columns
socio_demo_data <- rename(socio_demo_data, !!!new_names)


# Get the names of character columns, because gem_19 column is integer,
#we cannot replace empty rows with NA for that

char_cols <- names(socio_demo_data)[sapply(socio_demo_data, is.character)]
char_cols


# Only replace "" with NA in those columns
socio_demo_data <- socio_demo_data %>%
  mutate(across(all_of(char_cols), ~na_if(., "")))





# Count the number of "0,00" values in each column
zero_counts <- apply(socio_demo_data, 2, function(x) sum(x == "0,00", 
                                                         na.rm = TRUE))

print(zero_counts)

# Calculate the percentage of "0,00" values in each column
zero_percentages <- apply(socio_demo_data, 2, function(x) {
  zero_count <- sum(x == "0,00", na.rm = TRUE)
  non_zero_count <- sum(x != "0,00", na.rm = TRUE)
  
  # Return the percentage
  (zero_count / (zero_count + non_zero_count)) * 100
})

print(zero_percentages) #Kinosäle, Krankenhäuser.insgesamt , 
#U..Straßenbahn.Abfahrten,
#Bahnhaltestellen in all these column '0,00' values make up over 70% of the 
#total values, thus i will drop them , because more likely the variables
#will suffer from lack of variablity, This could potentially violate the
#assumption of normally distributed errors in my regression model
#for now i keep Polizeidienststellen(64%).



###-.......deleting some columns

socio_demo_data<- subset(socio_demo_data, 
                         select = -c(Kinosäle, Krankenhäuser.insgesamt,
                                     U..Straßenbahn.Abfahrten,
                                     TrainStations,Aggregat))




#next step convert integer gem_19 to character, converting some character 
#columns to numeric, changing decimal point from (,) to (.)


# I want to delete ,00 for values of the "CentralLocationStatus", 
# "CityType" columns and Specify the columns to mutate

cols_to_mutate <- c("CentralLocationStatus", "CityType")

# Remove the comma and trailing zeros
socio_demo_data <- socio_demo_data %>%
  mutate_at(vars(cols_to_mutate), ~gsub(",00", "", .))

# Convert back to character
socio_demo_data <- socio_demo_data %>%
  mutate_at(vars(cols_to_mutate), ~as.character(.))



# I want to replace decimal separator with dot for all columns below
#and convert columns to numeric

columns_to_mutate <- c("Population", "PopulationDensity", 
                       "SettlementAndTrafficArea", 
                       "SettlementDensityPerKm", "PoliceStations",
                       "EmploymentDensity_AO", 
                       "EmploymentDensity_WO", "WorkplaceCentrality", 
                       "Unemployment", 
                       "BusStops", "HighwayAccessibility", 
                       "AirportAccessibility", "UpperCentreAccessibility", 
                       "PublicTransportStops", "ResidentsPerDoctor", 
                       "StudentsPer1000Residents")

# Remove the thousands separator, replace the comma with a period 
#and convert to numeric



socio_demo_data <- socio_demo_data %>%
  mutate_at(vars(columns_to_mutate), ~as.numeric(gsub(",", ".", 
                                                      gsub("\\.", "", .))))

sapply(socio_demo_data, class)

###gem_19 is as integer that i will convert to character since it contains info
#about the ID codes of the municipalities


#socio_demo_data$gem_19 <- as.character(socio_demo_data$gem_19)


socio_demo_data <- socio_demo_data %>%
  mutate(CityType = as.character(CityType),  CentralLocationStatus = 
           as.character(CentralLocationStatus,
                        gem_19 =as.character(gem_19)))


socio_demo_data <- socio_demo_data %>% mutate(gem_19 = as.character(gem_19))


#merging filtered house data with socio demoghrapic data
#first i merged the two datasets, there 1,6mil unmatched values
#by checking unique values i detected some whitespaces, so i will first
#remove those before i merge the two datasets



unique_values <- unique(dataaa_filtered_munici_codes$gem19)
unique_values


# for testing i tried to find matches for the value equals to "1001000"
#i could not find exact match, thus i used partial string matching technique , 
#i found a match. Meaning that there spaces and so on interrupts for
#exact matches

count <- sum(grepl("1001000", dataaa_filtered_munici_codes$gem19))
count

dataaa_filtered_munici_codes$gem19 <- str_trim(dataaa_filtered_munici_codes$gem19)


dataaa_filtered_socio_demo <- dataaa_filtered_munici_codes %>%
  left_join(socio_demo_data, by = c("gem19" = "gem_19"))



#...................Reading House Geocoordinates dataset........
 

#house_geo_points <-st_read("grid_centroid.geojson")

#saveRDS(house_geo_points , "house_geo_points.rds")

house_geo_points <- readRDS("house_geo_points.rds")

length(unique(house_geo_points$r1_id))

#changing col name to r1_id

colSums(is.na(dataaa_filtered_socio_demo))

colnames(house_geo_points)[which(names(house_geo_points) ==
                                   "idm")] <- "r1_id" 


#merging house dataset with house coordinates

house_data <- left_join(dataaa_filtered_socio_demo,
                        house_geo_points, by = "r1_id")


#converting house_data to sf object
st_geometry(house_data) <- house_data$geometry  



#-------------------------------------------------------------------------------

munici_data <- readRDS("munici_data.rds")

# Subset munici_data
munici_data_sub <- munici_data[c("municipality_code",
                                 "Stadt- und Gemeindetyp (Bundesländer) Name")]

# Join with house_data_sample
house_data <- left_join(house_data, munici_data_sub, 
                         by = c("gem19" = "municipality_code"))



colnames(house_data)[which(names(house_data) == 
                              "Stadt- und Gemeindetyp (Bundesländer) Name")] <-
  "state_muniType"


#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------

#.......Creating some other variables for regression........................


 

house_data <- house_data %>%
  mutate(Age = year(date_finish) - construction_year)  

# Subset the data frame to include only rows where Age is greater 
#than or equal to 0

house_data <- house_data[house_data$Age >= 0, ]


house_data$Age_sqrt <- house_data$Age^2


house_data$number_rooms_sqrt <- house_data$number_rooms^2

house_data$bedroom_sqrt <- house_data$bedroom^2

house_data$parkplace_dummy <- ifelse(house_data$parkplace == 1, 1, 0)

house_data <- house_data %>%
  mutate(year = year(ymd(date_finish)))


#-------------------------------------------------------------------------------

# removing some columns we don't need anymore

house_data  <- subset(house_data, select = - c(elevator,
                                               change_on_previous_years_month,
                                                change_on_previous_month ,
                                                cons_pr_index))

house_data  <- subset(house_data, select = - ResidentsPerDoctor)

# Replacing NA values with unknown_ in character columns

house_data  <- house_data  %>%
  mutate_at(vars(AGS_gem0, gem_name, gem19rs, gem19,AGS, municipality_name,
                 state_muniType), 
            ~ ifelse(is.na(.), paste0("unknown_", colnames(.)), .))


colSums(is.na(house_data))
 
na.omit(house_data)
 
#-------------------------------------------------------------------------------

# saveRDS(house_data, "house_data.rds")

 

 
 
 
#----------------------------------------------------------------------



