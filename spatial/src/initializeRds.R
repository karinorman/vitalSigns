library(sp)
library(raster)
library(spatstat)
library(maptools)
library(rgdal)
library(rworldmap)
library(rgeos)
library(doParallel)
library(geometry)
setwd("~/Dropbox/vitalSigns/analysis/vital_signs/spatial")
## source('src/CalcRdsStats.R')

data.path <- '~/Dropbox/vitalSigns/data/spatial/'
save.path <- '~/Dropbox/vitalSigns/saved/spatial/'

#### Read in data
farm <- na.omit(read.csv(file.path(data.path,'landscape.csv')))
tanz.farm <- farm[farm$country == "TZA",]
gha.farm <- farm[farm$country == "GHA",]
ug.farm <- farm[farm$country == "UGA",]
rwa.farm <- farm[farm$country == "RWA",]

#### Read in road data
## roads <- readOGR(dsn = "C:/Users/Katherine Siegel/Dropbox/vitalSigns/data/spatial/roads/gROADS-v1-africa-shp",
##                  layer = "gROADS-v1-africa")

#### Save road data to dropbox
## save(roads, file = file.path(save.path, "roads.Rdata"))

#### Load road data
load("~/Dropbox/vitalSigns/saved/spatial/roads.Rdata")

#### Get country maps from rworldmaps
data(countriesLow)
country_map <- countriesLow

#### Subset Vital Signs countries
## GHA, TZA, UGA
vs_countries <- country_map[country_map@data$ISO3 %in% c("RWA",
                                                     "TZA", "UGA",
                                                     "GHA"),]

#### Make dataframe of country names to extract from world map
## df <- NULL
## df$country <- c("Ghana", "Rwanda", "Tanzania", "Uganda")
## df$code <- c("GHA", "RWA", "TZA", "UGA")
## df <- as.data.frame(df)
## vs_countries <- joinCountryData2Map(df, joinCode = "ISO3", nameJoinColumn = "code")

#### Put vs_countries in same projection as roads
vs_countries <-  spTransform(vs_countries, crs(roads))

#### Extract coordinates from vs_countries
gha_coord <- vs_countries@polygons[[1]]@Polygons[[1]]@coords
tanz_coord <- rbind(vs_countries@polygons[[2]]@Polygons[[1]]@coords,
                    vs_countries@polygons[[2]]@Polygons[[2]]@coords,
                    vs_countries@polygons[[2]]@Polygons[[3]]@coords,
                    vs_countries@polygons[[2]]@Polygons[[4]]@coords)
uga_coord <- vs_countries@polygons[[3]]@Polygons[[1]]@coords
rwa_coord <- vs_countries@polygons[[4]]@Polygons[[1]]@coords

#### Combine into single file with all coordinates
all_coords <- rbind(gha_coord, tanz_coord, uga_coord, rwa_coord)

#### Try code on TZ roads
## TZ roads
## tz_rd <- roads[roads@data$SOURCEID == "s035_0001", ]
#### extract coords
## tz_rds_coord <- coordinates(tz_rd)
## tz_rds_coord <- sapply(tz_rds_coord, function(x) x[[1]])
## tz_rds_coord <- do.call(rbind, tz_rds_coord)
##
###### Extract coordinates from all roads
## rds <-  coordinates(roads)
## rds <- sapply(rds, function(x) x[[1]])
## rds <- do.call(rbind, rds)

#### Subset data sources used for VS countries
source_ids <- c("s005_000x", "s011_0001", "s012_000x", "s017_000x", "s022_0001",
                "s027_0000", "s030_000x", "s031_000x", "s034_000x", "s035_0001")
vs_road <- roads[roads@data$SOURCEID %in% source_ids, ]
## Extract coordinates from vs_road
vs_rd_coord <- coordinates(vs_road)
vs_rd_coord <- sapply(vs_rd_coord, function(x) x[[1]])
vs_rd_coord <- do.call(rbind, vs_rd_coord)

#### Intersection function
myIntersector <- function(polygon, lines, cores) {

    registerDoParallel(cores)

    ## find the convex hull of the outline and its area
    hull <- geometry::convhulln(polygon, "FA")

    ## extract just the points that make up the hull
    uniquePts <- unique(as.vector(hull[[1]]))

    ## give polygon row names for subsetting
    row.names(polygon) <- 1:dim(polygon)[1]

    ## find the actual points using the indices
    actualPts <- polygon[row.names(polygon) %in% uniquePts, ]

    ## for loop where for each pt in lines, it rbinds to actualPts
    ## if the area of the convex hull increases, return a 1 (otherwise return 0)
    findCutOut <- function(i, actualPts, lines){
        ## create temp dataframe where it binds the pt onto the pts in the convex hull
        temp <- rbind(actualPts, lines[i, ])

        ## find the new convex hull
        newHull <- geometry::convhulln(temp, "FA")

        ## if the new convex hull is greater than the old area, set cutOut to 0 (else: 1)
        if(newHull$area > hull$area) {
            temp2 <- 0
        } else {
            temp2 <- 1
        }
        return(temp2)
    }

    ## cutOut <- mclapply(1:nrow(lines), findCutOut, actualPts, lines, mc.cores=cores)

    cutOut <- foreach(i=1:nrow(lines)) %dopar%  {
        findCutOut(i, actualPts, lines)
    }

    ## bind cutOut to lines, then cut any points from lines where cutOut = 0
    results <- cbind(lines, cutOut)
    results <- results[results[, "cutOut"] == 1, ]

    ## get rid of the cutOut column and return
    results <- results[, 1:2]
    results
}

#### Test function: TZ roads
## roads_in_tz <- myIntersector(polygon = all_coords, lines = tz_rds_coord, cores = 1)

#### Extract road points that are in VS countries
roads_in_vs <- myIntersector(polygon = all_coords,
                             lines =
                                                       vs_rd_coord,
                             cores = ncores)





