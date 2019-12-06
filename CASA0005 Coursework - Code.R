datatypelist <- data.frame(cbind(lapply(Schools,class)))


BNG = "+init=epsg:27700"

BoroughMap <- EW[grep("^E09",EW@data$lad15cd),]

addProviderTiles(providers$Stamen.TonerLines)
addProviderTiles(providers$Stamen.TonerLabels)
  
  var markers = L.markerClusterGroup({
    iconCreateFunction: function (cluster) {
      var childMarkers = cluster.getAllChildMarkers();
      // count how many there are of each class
      var counts = _.countBy(childMarkers, function(marker) {
        // class at icon level
        //return marker.options.icon.options.className;
        // class inside html
        return $(marker.options.icon.options.html).attr('class');
      });
      // get the class with the highest count
      var maxClass = _.invert(counts)[_.max(counts)];
      // use this class in the cluster marker
      return L.divIcon({ html: cluster.getChildCount(), className: maxClass });
    },
  });