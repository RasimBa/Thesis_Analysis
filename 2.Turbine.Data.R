

#.........Wind Turbine Data; Data Cleaning and Manupulation


mydata <- read.csv("turbines.csv", header = TRUE, sep = ';')



#finding empty columns
#names(mydata)[empty_cols]
empty_count <- sapply(mydata, function(x) sum(x == ""))

# Print the counts
print(empty_count)

mydata <- subset(mydata, select = -c(Zuschlagnummer..EEG.KWK.Ausschreibung.,
                                     Spannungsebene
))
mydata <- subset(mydata, select = -c(Elektrische.KWK.Leistung,
                                     Inbetriebnahmedatum.der.KWK.Anlage,
                                     Thermische.Nutzleistung.in.kW,
                                     MaStR.Nr..der.KWK.Anlage,
                                     Installierte.Leistung,
                                     EEG.Anlagenschlüssel,
                                     MaStR.Nr..der.EEG.Anlage,
                                     Netzbetreiberprüfung,
                                     Name.des.Anschluss.Netzbetreibers,
                                     MaStR.Nr..des.Anschluss.Netzbetreibers,
                                     MaStR.Nr..der.Genehmigung,
                                     Volleinspeisung.oder.Teileinspeisung,
                                     X.MaStR.Nr..des.Anlagenbetreibers,
                                     Name.des.Anlagenbetreibers..nur.Org..
                                     
))
mydata <- subset(mydata, select = -c(MaStR.Nr..der.Lokation,
                                     Technologie.der.Stromerzeugung,
                                     Lage.der.Einheit,
                                     Nutzbare.Speicherkapazität.in.kWh,
                                     Hersteller.der.Windenergieanlage,
                                     Typenbezeichnung,
                                     Hauptbrennstoff.der.Einheit,
                                     Name.des.Windparks,
                                     Anzahl.der.Solar.Module,
                                     Hauptausrichtung.der.Solar.Module,
                                     Anzeige.Name.der.Einheit,
                                     Energieträger
))

#mydata <- subset(mydata, select = -Gemeindeschlüssel)

mydata <- subset(mydata, select = -c(Hausnummer, Straße))

mydata <- subset(mydata, select = -Datum.der.geplanten.Inbetriebnahme)

mydata <- subset(mydata, select = -Datum.der.endgültigen.Stilllegung)

#mydata <- subset(mydata, select = -c(Flurstück, Gemarkung
#))
mydata <- subset(mydata, select = -Letzte.Aktualisierung)


mydata <- subset(mydata, select = -Bruttoleistung.der.Einheit
)

##### changing column names

colnames(mydata)[which(names(mydata) == 
                         "Koordinate..Breitengrad..WGS84.")] <- "lati"
colnames(mydata)[which(names(mydata) == 
                         "Koordinate..Längengrad..WGS84.")] <- "longi"
colnames(mydata)[which(names(mydata) == 
                         "Ort")] <- "city"
colnames(mydata)[which(names(mydata) == 
                         "Postleitzahl")] <- "postcode"
colnames(mydata)[which(names(mydata) ==
                         "Betriebs.Status")] <- "oper_status"
colnames(mydata)[which(names(mydata) ==
                         "Nabenhöhe.der.Windenergieanlage")] <- "hub.height"
colnames(mydata)[which(names(mydata) ==
                         "Registrierungsdatum.der.Einheit")] <- "reg.date.unit"

colnames(mydata)[which(names(mydata) ==
                         "Inbetriebnahmedatum.der.Einheit")] <- "unit_date.install"
colnames(mydata)[which(names(mydata) ==
                         "Inbetriebnahmedatum.der.EEG.Anlage")] <- "plant_date.install"
colnames(mydata)[which(names(mydata) ==
                         "Rotordurchmesser.der.Windenergieanlage")] <- "d_rotor"
colnames(mydata)[which(names(mydata) ==
                         "reg.data.unit")] <- "reg.date.unit"
colnames(mydata)[which(names(mydata) == "MaStR.Nr..der.Einheit")] <- "turbine_id"

colnames(mydata)[which(names(mydata) == "Gemeindeschlüssel")] <- "municipality_code"

colnames(mydata)[which(names(mydata) == "Gemarkung")] <- "district"
colnames(mydata)[which(names(mydata) == "Bundesland")] <- "federal_state"
colnames(mydata)[which(names(mydata) == "Flurstück")] <- "parcel"
colnames(mydata)[which(names(mydata) == "Nettonennleistung.der.Einheit")] <- "net_rated_power"
######changing values in oper_status column 

mydata[mydata=="In Betrieb"] <- "In Operation"
mydata[mydata == "In Planung"] <- "In Planning"
mydata[mydata == "Endgültig stillgelegt"] <- "Closed"
mydata[mydata == "Vorübergehend stillgelegt"] <- "Temporarily Shut Down"



#### dealing with missing values


sum(is.na(mydata))
colSums(is.na(mydata))

mydata[!complete.cases(mydata),]

#mydata[!(is.na(mydata$postcode) | mydata$postcode==""), ] #to removeblank cells
#for specific columns
#mydata[!(is.na(mydata$lati) | mydata$lati==""), ]
#mydata[!(is.na(mydata$longi) | mydata$longi==""), ]
#mydata[!(is.na(mydata$lati) | mydata$lati==""), ]

mydata[mydata == ''] <- NA #replacing all empty cells with NAs
na.omit(mydata) #omitting NA values
colSums(is.na(mydata)) #shows sum of NAs for each column
#mydata <- mydata %>% drop_na() #drops all NAs from the dataframe





#####lubridate, as_data

#lubridate::as_date(mydata$unit_date.install)




mydata$longi <- str_replace_all(mydata$longi, ",", ".") #substituting commas with dots
mydata$lati <- str_replace_all(mydata$lati, ",", ".") #since coordinate system has to be 
# coded with dots.


mydata$longi <- as.numeric(mydata$longi) #converting characters to numeric values
mydata$lati <- as.numeric(mydata$lati)
str(mydata)


mydata_no_na <- mydata %>% drop_na(longi, lati, unit_date.install) #droping NAs only for longi
#and lati columns in order to use sf function, we left with 24614 observations.
empty_counts_ <- sapply(mydata_no_na, function(x) sum(x == ""))

# Print the counts
print(empty_counts_)

colSums(is.na(mydata_no_na))
nrow(mydata_no_na)
# Convert the data frames to sf objects


wind_turbines_sf <- st_as_sf(mydata_no_na , coords = c("longi", "lati"), 
                             crs = 4326)


na.omit(wind_turbines_sf)


table(wind_turbines_sf$oper_status)



#......Since turbine data contains municipality codes, i merge it with 
#other dataset in order to identify whether a turbine is located in urban or
#rural areas



excel_file <- "Referenz Gemeinden, Kreise, NUTS.xlsx"

# Specify the name of the sheet you want to extract
sheet_name <- "Gemeinden-Gemeindeverbände"

# Read the sheet from the Excel file
municipality_data <- read_excel(excel_file, sheet = sheet_name)

# Create a CSV file name based on the sheet name
csv_file <- paste0(sheet_name, ".csv")

# Write the data to a CSV file
write.csv(municipality_data, file = csv_file, row.names = FALSE)

# Read the data again, this time skipping the first row
munici_data <- read.csv("Gemeinden-Gemeindeverbände.csv", skip = 1, 
                        header = FALSE, sep = ',')

#saveRDS(munici_data, "munici_data.rds")

colnames(munici_data) <- munici_data[1,]

# Remove the second row from the data
munici_data <- munici_data[-1,]

rownames(munici_data) <- NULL
colnames(munici_data)

colnames(munici_data)[which(names(munici_data) == 
                              "Gemeinden Kennziffer")] <- "municipality_code"


class(wind_turbines_sf$municipality_code)
class(munici_data$municipality_code)

#.............Because municipality_code in the turbine dataset is
#... integer i will convert it intocharacter before i merge thw two data sets

#convert to character
wind_turbines_sf$municipality_code <- as.character(wind_turbines_sf$municipality_code)

merged_wind_turbines_sf <- merge(wind_turbines_sf, munici_data, by = 
                                   "municipality_code")
#View(merged_wind_turbines_sf)


unmatched_rows <- anti_join(wind_turbines_sf, munici_data, 
                            by = "municipality_code")
#View(unmatched_rows)

# we lost over 1000 observations some due to NA values in municipality_code 
#column of the turbine data, and some due to unmatched codes.




colnames(merged_wind_turbines_sf)[which(names(merged_wind_turbines_sf) == 
                                          "Großstadtregionaler Einzugsbereich Name")] <-
  "geographic_area_type"


colnames(merged_wind_turbines_sf)[which(names(merged_wind_turbines_sf) == 
                                          "Zentralörtliche Einstufung (zusammengefasst) Name")] <-
  "central_place_classification"



#chaning some value names in specific needed columns


merged_wind_turbines_sf$geographic_area_type[merged_wind_turbines_sf$geographic_area_type==
                                               "Ergänzungsgebiet"] <- "supplement area"

merged_wind_turbines_sf$geographic_area_type[merged_wind_turbines_sf$geographic_area_type == 
                                               "Zentrum"] <- "center"

merged_wind_turbines_sf$geographic_area_type[merged_wind_turbines_sf$geographic_area_type == 
                                               "engerer Pendlerverflechtungsbereich"] <- "closer commuter connection area"

merged_wind_turbines_sf$geographic_area_type[merged_wind_turbines_sf$geographic_area_type == 
                                               "nicht zu Großstadtregion gehörend"] <- "not belonging to metropolitan area"

merged_wind_turbines_sf$geographic_area_type[merged_wind_turbines_sf$geographic_area_type ==
                                               "weiterer Pendlerverflechtungsbereich"] <- "further commuter integration area"




merged_wind_turbines_sf$central_place_classification[merged_wind_turbines_sf$central_place_classification==
                                                       "Oberzentrum und höher"] <- "High Central Place"

merged_wind_turbines_sf$central_place_classification[merged_wind_turbines_sf$central_place_classification == 
                                                       "Mittelzentrum"] <- "Middle Central Place"

merged_wind_turbines_sf$central_place_classification[merged_wind_turbines_sf$central_place_classification == 
                                                       "Grundzentrum und niedriger"] <- "Low Central Place"

merged_wind_turbines_sf$central_place_classification[merged_wind_turbines_sf$central_place_classification == 
                                                       "keine zentralörtliche Einstufun"] <- "No Central Place"





empty_count_final <- sapply(merged_wind_turbines_sf, function(x) sum(x == ""))

# Print the counts
print(empty_count_final) #there is no empty columns anymore, but NAs still are there


 

# saveRDS(merged_wind_turbines_sf, "turbine_final.rds")

turbine_final <- readRDS("turbine_final.rds")

 
