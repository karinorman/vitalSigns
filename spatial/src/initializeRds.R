library(sp)
library(raster)
library(spatstat)
library(maptools)
library(SDMTools)
library(rgdal)
library(rworldmap)
library(rgeos)
library(doParallel)
library(geometry)
source('src/CalcRdsStats.R')

data.path <- '~/Dropbox/vitalSigns/data/spatial/'
save.path <- '~/Dropbox/vitalSigns/saved/spatial/'

## Read in data
farm <- na.omit(read.csv(file.path(data.path,'landscape.csv')))
tanz.farm <- farm[farm$country == "TZA",]
gha.farm <- farm[farm$country == "GHA",]
ug.farm <- farm[farm$country == "UGA",]
rwa.farm <- farm[farm$country == "RWA",]

## Read in road data
# roads <- readOGR(dsn = "C:/Users/Katherine Siegel/Dropbox/vitalSigns/data/spatial/roads/gROADS-v1-africa-shp", 
#                  layer = "gROADS-v1-africa")
 
## Save road data to dropbox
# save(roads, file = file.path(save.path, "roads.Rdata"))

## Load road data
load("~/Dropbox/vitalSigns/saved/spatial/roads.Rdata")

## Get country polygons from raster package


## Get country maps from rworldmaps
data(countriesLow)
country_map <- countriesLow

## Make dataframe of country names to extract from world map
df <- NULL
df$country <- c("Ghana", "Rwanda", "Tanzania", "Uganda")
df$code <- c("GHA", "RWA", "TZA", "UGA")
df <- as.data.frame(df)
vs_countries <- joinCountryData2Map(df, joinCode = "ISO3", nameJoinColumn = "code")

## Use mask to remove road data from outside of VS countries
# rr <- mask(roads, vs_countries) ## doesn't work (polygon vs. line)
# vs_roads <- gIntersection(roads, vs_countries) ## takes a very long time!

vs_countries <-  spTransform(vs_countries, crs(roads))

vs_countries@polygons@Polygons@ID

## Extract coordinates from vs_countries
country_polygon <- vs_countries@polygons[[2]]@Polygons[[1]]@coords

## Extract coordinates from roads
rds <-  coordinates(roads)

rds <- sapply(rds, function(x) x[[1]])
rds <- do.call(rbind, rds)

## Intersection function
myIntersector <- function(polygon, lines, cores) {
  
  registerDoParallel(cores)
  
  # find the convex hull of the outline and its area
  hull <- geometry::convhulln(polygon, "FA")
  
  # extract just the points that make up the hull
  uniquePts <- unique(as.vector(hull[[1]]))
  
  # give polygon row names for subsetting
  row.names(polygon) <- 1:dim(polygon)[1]
  
  # find the actual points using the indices
  actualPts <- polygon[row.names(polygon) %in% uniquePts, ]
  
  # for loop where for each pt in lines, it rbinds to actualPts
  # if the area of the convex hull increases, return a 1 (otherwise return 0)
  findCutOut <- function(i, actualPts, lines){
    # create temp dataframe where it binds the pt onto the pts in the convex hull
    temp <- rbind(actualPts, lines[i, ])
    
    # find the new convex hull
    newHull <- geometry::convhulln(temp, "FA")
    
    # if the new convex hull is greater than the old area, set cutOut to 0 (else: 1)
    if(newHull$area > hull$area) {
      temp2 <- 0
    } else {
      temp2 <- 1
    }
  return(temp2)
  }
  
  # cutOut <- mclapply(1:nrow(lines), findCutOut, actualPts, lines, mc.cores=cores)

  cutOut <- foreach(i=1:nrow(lines)) %dopar%  {
                      findCutOut(i, actualPts, lines)
  }
  
  # bind cutOut to lines, then cut any points from lines where cutOut = 0
  results <- cbind(lines, cutOut)
  results <- results[results[, "cutOut"] == 1, ]
  
  # get rid of the cutOut column and return
  results <- results[, 1:2]
  results
}

roads_in_vs <- myIntersector(polygon = country_polygon, lines = rds, cores = 3)

