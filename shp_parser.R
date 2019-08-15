# Remove any variables in the workspace
rm(list=ls())
# Change the directory to current directory
setwd("C:\\temp\\javaapps\\gama_1105\\WSDOT_EV\\results")

require(rgdal)
shape_feasibility <- readOGR(dsn = ".", layer = "road_network_weighted2019-03-29-10-13-38")
shape_combo <- readOGR(dsn = ".", layer = "road_network_weighted_combo2019-04-01-03-51-32.shp")
library('sf')

# Load shapefile
shapeName <- read_sf('road_network_weighted2019-03-29-10-13-38.shp')
shapeName2 <- read_sf('road_network_trip_weighted2019-03-29-11-43-07.shp')
shapeName3 <- read_sf('road_network_weighted_combo2019-04-01-03-51-32.shp')
shapeName4 <- read_sf('rn_weighted_trip_inf_chademo_2019-04-05-01-29-27.shp')
library(sp)

shape_feasibility <- spTransform(shape_feasibility, CRS("+proj=longlat +datum=WGS84 +no_defs"))
shape_trip_feasibility <- readOGR(dsn = ".", layer = "rn_weighted_trip_inf_chademo_2019-04-06-12-51-30")
shape_trip_feasibility <- spTransform(shape_trip_feasibility, CRS("+proj=longlat +datum=WGS84 +no_defs"))
library(leaflet)


leaflet()  %>% addTiles() %>% addPolylines(data = shape_trip_feasibility, weight = shape_trip_feasibility$weight/50,col = 'red')
leaflet()  %>% addTiles() %>% addPolylines(data = shape_trip_feasibility[958,], weight = 10,col = 'green', popup = ~non_zero)

# Load shapefile
shapeName_Building <- read_sf('C:\\Users\\chintan\\Downloads\\GAMA1.8_EmbeddedJDK_Win_64bits_11.05.18_79f4df8\\configuration\\org.eclipse.osgi\\15\\0\\.cp\\models\\Features\\Save Statement\\results\\buildings.shp')
