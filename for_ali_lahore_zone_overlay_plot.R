library(readr)
library(dplyr)
library(tidyr)
library(data.table)
library(sp)

lahore_zone_data <- read_csv("~/Downloads/lahore-zone-data.csv")
setDT(lahore_zone_data)[, paste0("zone", 1:2) := tstrsplit(zone, "POLYGON\\(\\(")]
setDT(lahore_zone_data)[, paste0("zone", 3:4) := tstrsplit(zone2, "\\)\\)")]
reduntcolumns = c(paste0("zone",1:3))
lahore_zone_data <- subset(lahore_zone_data, select = -c(zone1,zone2,zone3))
colnames(lahore_zone_data)[3] <- "zone_adj"
lahore_zone_data$zone_cord_list <- strsplit(lahore_zone_data$zone_adj,",")
n <- dim(lahore_zone_data)[1] -2
ps_list = list()
for (xt in 1:n) {
  test1 <- lahore_zone_data$zone_cord_list[[xt]]
  test1 <- cbind(test1)
  test1 = as.data.frame(test1)
  setDT(test1)[, paste0("test1", 1:2) := tstrsplit(test1, " ")]
  test1 <- subset(test1,select = -c(test1))
  test1$x <- test1$test12
  test1$y <- test1$test11
  test1 <- subset(test1, select = -c(test11,test12)) #inefficient data cleaning prior
  test1 <- as.matrix(test1) 
  class(test1) <- "numeric"
  p = Polygon(test1)
  ps <- paste0('ps',xt)
  assign(ps,Polygons(list(p),lahore_zone_data[xt,1]))
  ps_list[[xt]] = Polygons(list(p),lahore_zone_data[xt,1])
}

sps = SpatialPolygons(ps_list,1:n)

proj4string(sps) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
#spdf = SpatialPolygonsDataFrame(sps,
plot(sps)


######ben's contribution
library(ggmap)

overlay = fortify(sps) #all this does is turn sps into a dataframe to allow us to plot easily
location <- geocode('Lahore, Pakistan') #defining a var
gmap <- get_map(location=location, zoom = 10, maptype = "roadmap", source = "google", col="bw") #downloading map from google maps

#plotting map using ggmap(), overlaying the polygons using geom_polygon() just like we do normally when we plot data - geom_point(), etc.
niceplot = ggmap(gmap) + geom_polygon(data = overlay,
                                                aes(x = long, y = lat, group = group),
                                                color = "red", fill = "red", alpha = .05) + theme_void()
niceplot
#i thought that was pretty sweet!


#but there's something off... i noticed there are 2 fewer zones than the original data set
length(unique(lahore_zone_data$name)) - length(unique(overlay$group))

#and that there may be a problem with gulshan ravi, perhaps
problem = ggmap(gmap) + geom_polygon(data = overlay %>% filter(id == "'Gulshan Ravi'"),
                                     aes(x = long, y = lat, group = group),
                                     color = "red", fill = "red", alpha = .05)
problem


#shrugs

#happy eid!




##more work from ben



#creating points that are somewhere in lahore,
points <- data.frame(long=c(74.2),
                     lat =c(31.3),
                     id  =c("A"), stringsAsFactors=F)
# checking if each point falls within a zone...this throws an error
gContains(sps,SpatialPoints(points))


# trying to do two at a time just for fun + scalability
# points <- data.frame(long=c(74.2,74.4), 
#                      lat =c(31.3,31.6),
#                      id  =c("A","B"), stringsAsFactors=F)
# 
# for (i in c(1:n)) {
#   list(id=points[i,]$id,
#        gContains(sps,SpatialPoints(points[i,1:2],proj4string=CRS(proj4string(sps))))) }




#all polys are self-valid, but there is a conflict between two or more polys
unique(gIsValid(sps, byid = T))
overlay %>% filter(long == 74.140548699999997)


#get list of all poly names from polygon shape
a = ls(getSpPPolygonsIDSlots(sps))

#get all unique combos
m_combos = combn(a,2) #matrix of matrices
combos = list(m_combos) #list of matrices
combos = list(combos[]) # now it is a list of lists

#creating a data frame of logical vectors for each of the 465 permutations of 
#note: elements are TRUE if there is an overlap, and FALSE if there is no overlap
overlap_matrix = base::as.data.frame(gOverlaps(sps,byid = T,checkValidity = F))
dim(overlap_matrix) #checking dimensions, looks good

overlap_matrix$number_of_overlaps = rowSums(overlap_matrix)

#i left the full data frame of logical vectors  in case its helpful to know which ones intersect
zone_overlaps = overlap_matrix %>% dplyr::select(number_of_overlaps)


#conclusion... i think the geo-coordinate data is fucked up, or we fucked up somewhere.

#if you cant change the geo-coordinate data there are workarounds i think.


#i put this point in the zone called Sundar industrial & sharif medical >>
points <- data.frame(long=c(74.2),
                     lat =c(31.3))

#but perhaps we can do it individually if we can't search the spatialpolygonsdataframe all at once.
#just a loop to figure out which 
for (i in c(1:n)){
  w = fortify(sps[i])
  ifelse(w$id == "'Sundar industrial & sharif medical'", print(i), print("0"))
} # this printed an 8, so i just want to check if it works with one point, searching one polygon at a time

#defining sundar as the 8th element of the SpatialPolygons object
sundar = sps[8]

#prints name of the zone that contains the point of interest
ifelse(gContains(sundar,SpatialPoints(points))==T,paste(sundar@polygons[[1]]@ID),paste("Not in Zone Area"))


#creating list of names of each zone
l = list()
for (i in c(1:n)){
  j = i
  l[[i]] <- gsub("'","",paste(sps@polygons[[i]]@ID))
  }





# first extract all spatialpolygons from sps, then list them and for loop your way to heaven

for(i in 1:n) {
  nam <- paste0("q",i)
  assign(nam, sps[i])}  
  
ls(pattern = "^q.*$")

ben = list()
ben = ben[1:n]
ben[1:n] = c(paste0("q",1:n))
names(ben) = paste(l)

concerto = lapply(ben,FUN = get) #AND NOW WE HAVE A LIST OF SPATIAL POLYGONS 



#defining points... i could write a function to do this but i'm tired
points <- data.frame(long=c(74.2),
                     lat =c(31.3), stringsAsFactors=F)

#the magic function.. this will tell you where your point is
wheres_waldo <- function(point_coordinates) {
  for (i in 1:n){
  if (gContains(concerto[[i]],SpatialPoints(point_coordinates))==T) {
              requiem = paste("Point within", gsub("'","",names(concerto[[i]])), "zone")
  break}
    
  else{
              requiem = paste("Point not within any zones")
  }          
  
  }
  print(requiem)
  print("Finished checking all zones!")
  }

#usage example
wheres_waldo(points)
