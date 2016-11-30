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
                        plt, ## plot data
                        rast, ## raster to calculate stats from
                        ## projections from
                        FUN=ClassStat, ## function for calculating
                        ## stats on buffers
                        plot="PLOT"){
  these.coord <- coordinates(plt[i, ])
  spStats <- vector("list", length=length(d))
  for(j in 1:length(d)){
    p <- spatstat:::disc(d[j], these.coord)
    p <- as(p, 'SpatialPolygons')
    proj4string(p) <- CRS(proj4string(plt))
    ## masking is more time intensive on larger
    ## rasters so crop first
    new.rast <- crop(rast, extent(p))
    new.rast <- mask(new.rast, p)
    spStats[[j]] <- FUN(new.rast)
  }
  names(spStats) <- d
  return(spStats)
}



###### example
d <- seq(from=50, to=250, by=50)
nplot <- nrow(coordinates(plots))

save.dir <- "output"
all.class.stats <- lapply(1:nplot, calcSpStats, d, plots, 
                          pyro.rast)

names(all.class.stats) <- plots@data$PLOT

load('~/Dropbox/Yosemite/analysis/spatialData/config/saved/rasters/allRast.Rdata')
load('~/Dropbox/Yosemite/analysis/spatialData/data/plots.Rdata')

d <- seq(10, 1000, by=100)
nplot <- nrow(plots@data)

testlapply <- lapply(1:nplot, calcSpStats, d = 10, 
                     plt = plots, rast =  pyro.rast, sev=sev,
                     file.name = 'output/tanz_stats.rdata', plot = 'PLOT')




##Tanzania analysis
#Read in data 
farm <- readOGR("data", "landscapes_20160927")
tanz.farm <- farm[farm$cartodb_id >12 & farm$cartodb_id < 21,]
tanz <- readGDAL("data/tz_tm1-57palglobdem_landcover.dat") 
tanz <- raster(tanz)

#Test Function
test.farm <- farm[farm$cartodb_id == 13,]
testlapply <- lapply(1:1, calcSpStats, d = 10, 
                     plt = test.farm, rast =  tanz)

nplot <- length(tanz.farm@polygons)
buff <- seq(10, 1000, by=100)
options(cores = 10)
#spstats.tanz <- mclapply(1:nplot, calcSpStats, d = 10, 
                         plt = tanz.farm, rast =  tanz)
#save(spstats.tanz, "output/tanz_stats.rdata")
spstats.mult.tanz <- mclapply(1:nplot, calcSpStats, d = buff, 
                              plt = tanz.farm, rast =  tanz)
save(spstats.mult.tanz, "output/tanz_stats_mult.rdata")

# ## map shapefile
# library(RColorBrewer)
# colors <- brewer.pal(9, "BuGn")
# library(ggmap)
# mapImage <- get_map(location = c(lon = 35, lat = -6),
#                     color = "color",
#                     source = "osm",
#                                         # maptype = "terrain",
#                     zoom = 6)
# landscape <- fortify(tanz.farm)
# ggmap(mapImage) +
#   geom_polygon(aes(x = long,
#                    y = lat,
#                    group = group),
#                data = landscape,
#                color = colors[9],
#                fill = colors[6],
#                alpha = 0.5) +
#   labs(x = "Longitude",
#        y = "Latitude")
