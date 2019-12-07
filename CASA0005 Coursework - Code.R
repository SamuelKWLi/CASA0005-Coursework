datatypelist <- data.frame(cbind(lapply(Schools,class)))


BNG = "+init=epsg:27700"

BoroughMap <- EW[grep("^E09",EW@data$lad15cd),]

addProviderTiles(providers$Stamen.TonerLines)
addProviderTiles(providers$Stamen.TonerLabels)
  
view(SPWards@data)

### Plot Map using tmap
# Change tmap mode.
tmap_mode("view")

# Set colour palette.


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

