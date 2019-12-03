datatypelist <- data.frame(cbind(lapply(Schools,class)))


BNG = "+init=epsg:27700"

BoroughMap <- EW[grep("^E09",EW@data$lad15cd),]