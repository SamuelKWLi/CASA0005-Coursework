# Check column data type.
datatypelist <- data.frame(cbind(lapply(Schools,class)))

# Select rows that start with a certain string of values.
BoroughMap <- EW[grep("^E09",EW@data$lad15cd),]

# Colour bins for MSOA point density.
bins = c(0, (getJenksBreaks(PopDataMSOA$`School (All) Density`, k=5)+0.001))



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





### Extract Mean Concentrations
# Extract mean pollution concentration.
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





### Plot Map using tmap
# Change tmap mode.
tmap_mode("view")

# Plot map of schools, GPs and hospitals.
Map <- tm_shape(SFLAD)+ 
  tm_polygons("LAD Name", col = NA, alpha = 0, border.col = "black", border.alpha = 0.5)+
  tm_shape(SFMSOA)+ 
  tm_polygons("MSOA Name", col = NA, alpha = 0, border.col = "black", border.alpha = 0.5)+
  tm_shape(ClusterSFMSOA)+ 
  tm_polygons("School (All) Moran's I", palette = PalMi, breaks = BrksMi, border.col = "black", border.alpha = 0.5)+
  tm_shape(SFLocations)+
  tm_dots(title = "Building Type (All)", col = "Building Type", palette = c("lightblue", "navy", "lightgreen"))+
  tm_shape(SFLocationsNO2)+
  tm_dots(title = "Building Type (NO2)", col = "Building Type", palette = c("red", "brown", "orange"))+
  tm_shape(SFLocationsPM2.5)+
  tm_dots(title = "Building Type (PM2.5)", col = "Building Type", palette = c("red", "brown", "orange"))+
  tm_layout(legend.show = TRUE)

Map





### Point Data Local Moran's I
# Conduct local Moran's I.
lclMIAllSchool <- round(localmoran(SPClusterLSOA@data$SchoolDensity, SpWeightsLSOA), digits = 3)
lclMIAllGP <- round(localmoran(SPClusterLSOA@data$GPDensity, SpWeightsLSOA), digits = 3)
lclMIAllHospital <- round(localmoran(SPClusterLSOA@data$HospitalDensity, SpWeightsLSOA), digits = 3)
lclMINO2School <- round(localmoran(SPClusterLSOA@data$SchoolNO2Density, SpWeightsLSOA), digits = 3)
lclMINO2GP <- round(localmoran(SPClusterLSOA@data$GPNO2Density, SpWeightsLSOA), digits = 3)
lclMINO2Hospital <- round(localmoran(SPClusterLSOA@data$HospitalNO2Density, SpWeightsLSOA), digits = 3)
lclMIPM2.5School <- round(localmoran(SPClusterLSOA@data$SchoolPM2.5Density, SpWeightsLSOA), digits = 3)
lclMIPM2.5GP <- round(localmoran(SPClusterLSOA@data$GPPM2.5Density, SpWeightsLSOA), digits = 3)
lclMIPM2.5Hospital <- round(localmoran(SPClusterLSOA@data$HospitalPM2.5Density, SpWeightsLSOA), digits = 3)

# Join local Moran's I Z values to shape file.
SPClusterLSOA$lclMIAllSchool <- lclMIAllSchool[,4]
SPClusterLSOA$lclMIAllGP <- lclMIAllGP[,4]
SPClusterLSOA$lclMIAllHospital <- lclMIAllHospital[,4]
SPClusterLSOA$lclMINO2School <- lclMINO2School[,4]
SPClusterLSOA$lclMINO2GP <- lclMINO2GP[,4]
SPClusterLSOA$lclMINO2Hospital <- lclMINO2Hospital[,4]
SPClusterLSOA$lclMIPM2.5School <- lclMIPM2.5School[,4]
SPClusterLSOA$lclMIPM2.5GP <- lclMIPM2.5GP[,4]
SPClusterLSOA$lclMIPM2.5Hospital <- lclMIPM2.5Hospital[,4]

# Convert SP LSOA data into a data frame.
SFClusterLSOA <- st_as_sf(SPClusterLSOA)

# Transform CRS to WGS
SFClusterLSOA <- st_transform(SFClusterLSOA, WGS84)

# Rename columns.
colnames(SFClusterLSOA) <- c("LSOA Name", "LSOA Code", "MSOA Name", "MSOA Code","LAD Name", "LAD Code", "School (All) Count", "GP (All) Count", "Hospital (All) Count", "School (NO2) Count", "GP (NO2) Count", "Hospital (NO2) Count", "School (PM2.5) Count", "GP (PM2.5) Count", "Hospital (PM2.5) Count", "School (All) Density", "GP (All) Density", "Hospital (All) Density", "School (NO2) Density", "GP (NO2) Density", "Hospital (NO2) Density", "School (PM2.5) Density", "GP (PM2.5) Density", "Hospital (PM2.5) Density", "School (All) Moran's I", "GP (All) Moran's I", "Hospital (All) Moran's I", "School (NO2) Moran's I", "GP (NO2) Moran's I", "Hospital (NO2) Moran's I", "School (PM2.5) Moran's I", "GP (PM2.5) Moran's I", "Hospital (PM2.5) Moran's I", "geometry")

# Moran's I breaks.
BrksMi <-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)

# Moran's I palette.
PalMi<- rev(brewer.pal(7, "RdBu"))

# Map Moran's I.
tm_shape(SFClusterLSOA) +
  tm_polygons(col = "School (All) Moran's I", border.alpha = 0.5, palette = PalMi, breaks = BrksMi)
tm_shape(SFClusterLSOA) +
  tm_polygons(col = "GP (All) Moran's I", border.alpha = 0.5, palette = PalMi, breaks = BrksMi)
tm_shape(SFClusterLSOA) +
  tm_polygons(col = "Hospital (All) Moran's I", border.alpha = 0.5, palette = PalMi, breaks = BrksMi)
tm_shape(SFClusterLSOA) +
  tm_polygons(col = "School (NO2) Moran's I", border.alpha = 0.5, palette = PalMi, breaks = BrksMi)
tm_shape(SFClusterLSOA) +
  tm_polygons(col = "GP (NO2) Moran's I", border.alpha = 0.5, palette = PalMi, breaks = BrksMi)
tm_shape(SFClusterLSOA) +
  tm_polygons(col = "Hospital (NO2) Moran's I", border.alpha = 0.5, palette = PalMi, breaks = BrksMi)
tm_shape(SFClusterLSOA) +
  tm_polygons(col = "School (PM2.5) Moran's I", border.alpha = 0.5, palette = PalMi, breaks = BrksMi)
tm_shape(SFClusterLSOA) +
  tm_polygons(col = "GP (PM2.5) Moran's I", border.alpha = 0.5, palette = PalMi, breaks = BrksMi)
tm_shape(SFClusterLSOA) +
  tm_polygons(col = "Hospital (PM2.5) Moran's I", border.alpha = 0.5, palette = PalMi, breaks = BrksMi)