######################
##### LIBRAIRIES #####
######################

library(sp)
library(raster)
library(rgeos)
library(rgdal)
library(raster)

###################
##### GENERAL #####
###################

# Définir un object géographique à partir d'un dataframe avec deux (ou trois) colonnes pour les coordonnées géographiques
coordinates(GPS) <- ~Longitude+Latitude

# Définir la projection d'une couche
proj4string(couche) <- "+proj=longlat +datum=WGS84 +no_defs" # générique, mais aussi projection(r) possible pour les rasters

#### PRINCIPALES REFERENCES SPATIALES ####
# CGS WGS84 : "+proj=longlat +datum=WGS84 +no_defs"
# Lambert 93 : "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
# Lambert 2 : "+proj=lcc +lat_1=46.8 +lat_0=46.8 +lon_0=0 +k_0=0.99987742 +x_0=600000 +y_0=2200000 +a=6378249.2 +b=6356515 +towgs84=-168,-60,320,0,0,0,0 +pm=paris +units=m +no_defs"
# Pour trouver toutes les projections : http://www.epsg-registry.org/

###################
##### RASTERS #####
###################

# Lire un raster
bathy <- raster("./1_raw/shp/ETOPO1_Bed_c_geotiff.tif")

# Projeter un raster
bathyProj <- projectRaster(from=bathy,to=r, crs=proj4string(MedLevin), progress='window')

# Créer un raster avec une étendue et une résolution
r <- raster(extent(MedLevin), crs=proj4string(MedLevin))
res(r) <- c(1530.28, 1882.25)

# Créer un raster à partir d'un data frame
# create spatial points data frame
spg <- df
coordinates(spg) <- ~ x + y
# coerce to SpatialPixelsDataFrame
gridded(spg) <- TRUE
# coerce to raster
rasterDF <- raster(spg)

# Faire des calculs sur un seul raster
bathy <- calc(bathy, function(x){ifelse(x>max(prof),NA,x)})

# Faire des calculs sur plusieurs rasters
r7 <- overlay(r1, r2, r3, r4, fun=function(a,b,c,d){return(a*b+c*d)} )

# Remplacer certaines valeurs d'un raster
r[extent(c(988439.9,999086.9,6243459,6258309))] <- NA
r[1,] <- 10 # remplacer tous les pixels de la première ligne par 10
r[,2] <- 100 # remplacer tous les pixels de la deuxième colonne par 100

# Récupérer les valeurs d'un raster
valeurs <- getValues(r)
getValues(r, row=10)

# Clipper un raster par un shapefile ou par une étendue géographique
bathy <- crop(bathy, ext) # avec "ext" un objet de type "extent"

# Masquer une partie d'un raster à partir d'un autre raster
reliefProj <- mask(reliefProj, mask=MedContour)

# Dégrader un raster
bathy <- aggregate(bathy, fact=10, fun=mean, expand=TRUE, na.rm=TRUE)

# Exporter un raster
writeRaster(bathyProj, filename="./2_processed/shp/bathy_proj", format="GTiff")





######################
##### SHAPEFILES #####
######################

# Lire un shapefile
LAYER <- ogrListLayers("./1_raw/shp/grid10km_inds_overlay5.shp")
MedLevin <- readOGR("./1_raw/shp/grid10km_inds_overlay5.shp",layer=LAYER)

# Structure d'un shapefile dans R
#shapefile
#  ...@data : dataframe correspondant à la table attributaire du shapefile
#  ...@coords : coordonnées de tous les points, lignes ou polygones de lobjet
#  ...@bbox : valeurs minimales et maximales des coordonnées du shapefile
#  ...@proj4string : Projection utilisée pour le shapefile
#  

# Projeter un shapefile
GPS <- spTransform(GPS, CRS("+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000
                            +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m
                            +no_defs"))

# Faire une intersection entre deux polygones
inter <- gIntersection(shp, MedLevinBathy[i,], drop_not_poly=TRUE)

# Supprimer des polygones par leur identifiant
shp <- shp[-which(shp@data$ID<100),]

# Supprimer des points par une étendue géographique
rect <- as(extent(c(988439.9,999086.9,6243459,6258309)), 'SpatialPolygons') 
proj4string(rect) <- "+proj=lcc +lat_1=49 +lat_2=44 +lat_0=46.5 +lon_0=3 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ov <- over(shpPoints,rect,returnlist=TRUE)
shpPoints <- shpPoints[-which(!is.na(ov)),]

# Calculer la surface d'un polygone
gArea(inter)

# Vérifier la validité d'un shapefile (erreurs de noeuds de polygones...)
gIsValid(shp)

# Simplifier la structure d'un shapefile avec une tolérance métrique n pour les noeuds
gSimplify(shp,n)

# Exporter un shapefile
writeOGR(GPS, "./2_processed", "GPS_sites_l93", driver="ESRI Shapefile", overwrite_layer=TRUE)






