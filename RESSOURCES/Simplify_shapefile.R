
library(rgdal)
library(rgeos)

shp <- readOGR("Polygone MasseDEauSouterraine_VEDL2013_FXX-shp/PolygMasseDEauSouterraine.shp", "PolygMasseDEauSouterraine")
shp100 <- gSimplify(shp, tol=100,topologyPreserve = TRUE)
data <- shp@data
shp100 <- SpatialPolygonsDataFrame(shp100, data)
writeOGR(obj = shp100, dsn = "Polygone MasseDEauSouterraine_VEDL2013_FXX-shp/Polygones_ME_light.shp",layer="Polygones_ME_light", driver="ESRI Shapefile",overwrite_layer=TRUE)

shp50 <- gSimplify(shp, tol=50)
data <- data.frame("ID"=1:length(shp50))
row.names(data) <- 0:(length(shp50)-1)
shp50 <- SpatialPolygonsDataFrame(shp50, data)
writeOGR(obj = shp50, dsn = "Polygone MasseDEauSouterraine_VEDL2013_FXX-shp/Polygones_ME_light2.shp",layer="Polygones_ME_light2", driver="ESRI Shapefile",overwrite_layer=TRUE)

shp30 <- gSimplify(shp, tol=30)
data <- data.frame("ID"=1:length(shp30))
row.names(data) <- 0:(length(shp30)-1)
shp30 <- SpatialPolygonsDataFrame(shp30, data)
writeOGR(obj = shp30, dsn = "Polygone MasseDEauSouterraine_VEDL2013_FXX-shp/Polygones_ME_light30.shp",layer="Polygones_ME_light30", driver="ESRI Shapefile",overwrite_layer=TRUE)

shp10 <- gSimplify(shp, tol=10, topologyPreserve = TRUE)
data <- data.frame("ID"=1:length(shp10))
row.names(data) <- 0:(length(shp10)-1)
shp10 <- SpatialPolygonsDataFrame(shp10, data)
writeOGR(obj = shp10, dsn = "Polygone MasseDEauSouterraine_VEDL2013_FXX-shp/Polygones_ME_light10.shp",layer="Polygones_ME_light30", driver="ESRI Shapefile",overwrite_layer=TRUE)

shp200 <- gSimplify(shp, tol=200, topologyPreserve = TRUE)
data <- shp@data
shp200 <- SpatialPolygonsDataFrame(shp200, data)
writeOGR(obj = shp200, dsn = "Polygone MasseDEauSouterraine_VEDL2013_FXX-shp/Polygones_ME_light200.shp",layer="Polygones_ME_light30", driver="ESRI Shapefile",overwrite_layer=TRUE)

shp1000 <- gSimplify(shp, tol=1000, topologyPreserve = TRUE)
data <- shp@data
shp1000 <- SpatialPolygonsDataFrame(shp1000, data)
writeOGR(obj = shp1000, dsn = "Polygone MasseDEauSouterraine_VEDL2013_FXX-shp/Polygones_ME_light1000.shp",layer="Polygones_ME_light30", driver="ESRI Shapefile",overwrite_layer=TRUE)

