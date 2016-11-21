library(sp)
library(raster)
library(spatstat)
library(maptools)
library(SDMTools)
library(rgdal)

## ClassStat is a function from the SDMTools package that calculates the frag stat metrics 

## creates buffers of a given size, calculates frag stat metrics
calcSpStats <- function(d, ## buffer RADIUS
                        plt, ## plot data
                        nplot, ## number of plots
                        rast, ## raster to calculate stats from
                        # sev.poly, ## spatial file to extract
                        ## projections from
                        file.name, ## name of the output file
                        FUN=ClassStat, ## function for calculating
                        ## stats on buffers
                        giv.names=TRUE, ## give the output names
                        plot="PLOT", ## name of the column to name
                        ## output with
                        data.dir){ ## directory to save the sata in
  spStats <- vector("list", length=nplot)
  for(i in 1:nplot){
    for(j in 1:length(d)){
      these.coord <- coordinates(plt[i, ])
      p <- spatstat:::disc(d[j], these.coord)
      p <- as(p, 'SpatialPolygons')
      #proj4string(p) <- CRS(proj4string(sev.poly))
      ## masking is more time intensive on larger
      ## rasters so crop first
      new.rast <- crop(rast, extent(p))
      new.rast <- mask(new.rast, p)
      spStats[[i]][[j]] <- FUN(new.rast)
    }
  }
  if(giv.names){
    names(spStats) <- plt@data[,plot]
  }
  save(spStats, file=file.path(data.dir, file.name))
  return(spStats)
}


farm <- readOGR("landscape_data/landscapes", "landscapes")
summary(farm)

tanz <- readGDAL("landscape_data/tanzania_landcover/tz_tm1-57palglobdem_prob.dat") 
plot(tanz)


