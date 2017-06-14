library(sp)
library(raster)
library(spatstat)
library(maptools)
library(SDMTools)
library(rgdal)

## check if center of polygon is in raster extent
pointrastcheck <- function(pt, rast){
  ptx <- pt[1]
  pty <- pt[2]
  rastcord <- bbox(rast)
  check <- ptx >= rastcord[1,1] & ptx <= rastcord[1,2] &
    pty >= rastcord[2,1] & pty <= rastcord[2,2]
  if (check == TRUE){
    return(TRUE)
  }else {
    return(FALSE)
  }
}

## ClassStat is a function from the SDMTools package that calculates the frag stat metrics 

## creates buffers of a given size, calculates frag stat metrics
calcSpStats <- function(i,
                        d, ## buffer RADIUS
                        plt, ## plot csv
                        plt.name, ## name of column with plot names (not a string)
                        rast, ## raster to calculate stats from
                        ## projections from
                        FUN=ClassStat, ## function for calculating
                        ## stats on buffers
                        plot="PLOT"){
  rast <- projectRaster(rast, crs = "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs", method = 'ngb')
  these.coord <- as.matrix(plt[i,12:13])
  these.coord <- project(these.coord, proj = "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  spStats <- vector("list", length=length(d))
  for(j in 1:length(d)){
    p <- try(spatstat:::disc(d[j], these.coord), silent=TRUE)
    if(inherits(p, "try-error")) browser()
    p <- as(p, 'SpatialPolygons')
    proj4string(p) <- CRS(proj4string(rast))
    ## masking is more time intensive on larger
    ## rasters so crop first
    new.rast <- crop(rast, extent(p))
    new.rast <- mask(new.rast, p)
    spStats[[j]] <- FUN(new.rast)
  }
  names(spStats) <- d
  return(spStats)
}


#Read in data 
farm <- na.omit(read.csv('landscape.csv'))
tanz.farm <- farm[farm$country == "TZA",]
gha.farm <- farm[farm$country == "GHA",]
ug.farm <- farm[farm$country == "UGA",]
rw.farm <- farm[farm$country == "RWA",]

tanz <- readGDAL("data/tz_tm1-57palglobdem_landcover.dat") 
tanz <- raster(tanz)

gha <- readGDAL("data/gh_tm1-57palglobdem_landcover.dat")
gha <- raster(gha)

ug <- readGDAL("data/ug_tm1-57palglobdem_landcover.dat")
ug <- raster(ug)

rw <- raster("data/ESACCI-LC-L4-LCCS-Map-300m-P1Y-2015-v2.0.7.nc")

#Test Function
##test.farm <- tanz.farm[2,]
##buff <- seq(10, 1100, by=100)
##testlapply <- lapply(1:13, calcSpStats, d = 100,
                    ## plt = tanz.farm, plt.name = "description", rast =  tanz)

buff <- seq(1000, 10000, by=1000)

#Tanzania Analysis
nplot <- nrow(tanz.farm)
spstats.mult.tanz <- lapply(1:nplot, calcSpStats, d = buff,
                              plt = tanz.farm, plt.name = 'description', rast =  tanz)
names(spstats.mult.tanz) <- tanz.farm$description
save(spstats.mult.tanz, file = "output/tanz_stats.rdata")

#Ghana Analysis
nplot <- nrow(gha.farm)
spstats.mult.gha <- lapply(1:nplot, calcSpStats, d = buff,
                            plt = gha.farm, plt.name = 'description', rast =  gha)
names(spstats.mult.gha) <- gha.farm$description
save(spstats.mult.gha, file = "output/gha_stats.rdata")

#Uganda Analysis
nplot <- nrow(ug.farm)
spstats.mult.ug <- lapply(1:nplot, calcSpStats, d = buff,
                            plt = ug.farm, plt.name = 'description', rast =  ug)
names(spstats.mult.ug) <- ug.farm$description
save(spstats.mult.ug, file = "output/ug_stats.rdata")

#Rwanda Analysis
nplot <- nrow(rw.farm)
spstats.mult.rw <- lapply(1:nplot, calcSpStats, d = buff,
                          plt = rw.farm, plt.name = 'description', rast =  rw)
names(spstats.mult.rw) <- rw.farm$description
save(spstats.mult.rw, file = "output/rw_stats.rdata")