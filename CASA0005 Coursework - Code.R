# Check column data type.
datatypelist <- data.frame(cbind(lapply(Schools,class)))

# Select rows that start with a certain string of values.
BoroughMap <- EW[grep("^E09",EW@data$lad15cd),]


### Read in Code Points
# List full file paths for all of the code point csv files.
CodePointsFiles <- list.files(path = "Data/Schools and Health/OS Point Codes/Data", full.names=TRUE)

# Run a function for each code point file path, reading the csvs into an object.
AllCodePoints <- lapply(CodePointsFiles,function(i){
  read.csv(i, header=FALSE, skip=4)
})

# Convert the object into a data frame.
AllCodePoints <- do.call(rbind.data.frame, AllCodePoints)

# Rename the columns.
colnames(AllCodePoints) <- c("Postcode",	"Positional_quality_indicator",	"Eastings",	"Northings",	"Country_code",	"NHS_regional_HA_code",	"NHS_HA_code",	"Admin_county_code",	"Admin_district_code",	"Admin_ward_code")

# Specify relevent columns.
AllCodePoints <- AllCodePoints[,c(1, 3, 4)]

# Remove spaces in postcodes
AllCodePoints$Postcode <- gsub(" ","", AllCodePoints$Postcode)

# Write complete dataframe into a csv file.
write.csv(AllCodePoints,"Data/Schools and Health/OS Point Codes/Data/AllCodePoints.csv", row.names=FALSE)


### Plot Map using tmap
# Change tmap mode.
tmap_mode("view")

# Plot map of schools, GPs and hospitals.
Map <- tm_shape(SFBoroughs)+ 
    tm_fill(col = NA, alpha = 0)+ 
    tm_borders(col = "black")+
  tm_shape(SFWards)+ 
    tm_fill(col = NA, alpha = 0)+ 
    tm_borders(col = "black", alpha = 0.5)+
  tm_shape(SFWards$`School Density`)+ 
    tm_fill(col = NA, alpha = 0)+ 
    tm_borders(col = "black", alpha = 0.5)+
  tm_shape(SFLocations)+
    tm_dots(title = "Building Type (All)", col = "Building Type", palette = c("lightblue", "navy", "lightgreen"))+
  tm_shape(SFLocationsNO2)+
   tm_dots(title = "Building Type (NO2)", col = "Building Type", palette = c("red", "brown", "orange"))+
  tm_shape(SFLocationsPM2.5)+
    tm_dots(title = "Building Type (PM2.5)", col = "Building Type", palette = c("red", "brown", "orange"))+
  tm_layout(legend.show = TRUE)

Map

### Extract Mean Concentrations
# Extract mean Wards pollution concentration.
SFLAD$`Mean NO2 Conc.` <- extract(PollutionLondon[[1]], SFLAD, fun=mean, na.rm=TRUE) 
SFLAD$`Mean PM2.5 Conc.` <- extract(PollutionLondon[[2]], SFLAD, fun=mean, na.rm=TRUE) 
SFMSOA$`Mean NO2 Conc.` <- extract(PollutionLondon[[1]], SFMSOA, fun=mean, na.rm=TRUE) 
SFMSOA$`Mean PM2.5 Conc.` <- extract(PollutionLondon[[2]], SFMSOA, fun=mean, na.rm=TRUE) 
SFLSOA$`Mean NO2 Conc.` <- extract(PollutionLondon[[1]], SFLSOA, fun=mean, na.rm=TRUE) 
SFLSOA$`Mean PM2.5 Conc.` <- extract(PollutionLondon[[2]], SFLSOA, fun=mean, na.rm=TRUE) 
SFWards$`Mean NO2 Conc.` <- extract(PollutionLondon[[1]], SFWards, fun=mean, na.rm=TRUE) 
SFWards$`Mean PM2.5 Conc.` <- extract(PollutionLondon[[2]], SFWards, fun=mean, na.rm=TRUE) 

# Copy SF objects.
ConcLAD <- SFLAD
ConcMSOA <- SFMSOA
ConcLSOA <- SFLSOA
ConcWards <- SFWards

# Remove geometry.
st_geometry(ConcLAD) <- NULL
st_geometry(ConcMSOA) <- NULL
st_geometry(ConcLSOA) <- NULL
st_geometry(ConcWards) <- NULL

# Write csv files.
write.csv(ConcLAD,"Data/Air Pollution/ConcLAD.csv", row.names=FALSE)
write.csv(ConcMSOA,"Data/Air Pollution/ConcMSOA.csv", row.names=FALSE)
write.csv(ConcLSOA,"Data/Air Pollution/ConcLSOA.csv", row.names=FALSE)
write.csv(ConcWards[,c(1, 2, 3, 4, 23, 24)],"Data/Air Pollution/ConcWards.csv", row.names=FALSE)
