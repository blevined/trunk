library(rgdal)
library(raster)
library(sp)
library(tidyverse)


lahore = read.csv("~/Downloads/lahore-zone-data.csv")


#character variable cleanup
lahore$name = as.character(lahore$name)
lahore$zone = as.character(lahore$zone)
lahore$zone = gsub("POLYGON\\(\\(","",lahore$zone, ignore.case = T)
lahore$zone = gsub("\\)\\)","",lahore$zone, ignore.case = T)

#splitting out coordinates pairs into individual observations
lahore = lahore %>% mutate(V3 = strsplit(as.character(zone), ",")) %>% unnest(V3)

lahore$lat = gsub(" .*$","",lahore$V3)
lahore$long = gsub(".* ", "",lahore$V3)


#i kept the 'zone' and created V3 vectors to check my work, but they are unnecessary so i drop them
lahore = lahore %>% dplyr::select(-V3,-zone) %>% mutate(lat = as.numeric(lat), long = as.numeric(long))

xy = lahore[,c(2,3)]

spdf = SpatialPointsDataFrame(coords = xy,
                              data = lahore,
                              proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))



### below this is broken
#helpful links:
#https://rstudio-pubs-static.s3.amazonaws.com/172289_67a42eebbd574197b6bb15d1ef6cfe97.html
#https://stackoverflow.com/questions/21759134/convert-spatialpointsdataframe-to-spatialpolygons-in-r
##the above should have what you want!




asdf <- data.frame(name=(lahore$name),row.names=row.names(lahore))

get.grpPoly <- function(group,ID,df) {
  Polygon(coordinates(df[df$id==ID & df$name==group,]))
}


get.grpPoly(name,)


points2polygons <- function(df,data) {
  get.grpPoly <- function(group,ID,df) {
    Polygon(coordinates(df[df$id==ID & df$name==group,]))
  }
  get.spPoly  <- function(ID,df) {
    Polygons(lapply(unique(df[df$id==ID,]$name),get.grpPoly,ID,df),ID)
  }
  spPolygons  <- SpatialPolygons(lapply(unique(df$id),get.spPoly,df))
  SpatialPolygonsDataFrame(spPolygons,match.ID=T,data=data)
}
spacepoly <- points2polygons(spdf,asdf)
plot(spDF,col=spDF$box_id+1)


