## Function to calculate the total population density for a buffer
calcPopStats <- function(i,
                        d, ## buffer RADIUS
                        plt, ## plot csv
                        rast, ## raster to calculate stats from
                        ## projections from
                        FUN = sum, ## function for summing pop densities in buffers
                        ## with plot data
                        coordinate.cols = c("centerpoint_longitude", "centerpoint_latitude"),
                        ## crs =
                        ##     "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                        proj =
                            "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"){

    ## Create buffers of a given size, then calculate total population density in buffers
    ## rast <- projectRaster(rast, crs = crs, method = 'ngb')
    these.coord <- as.matrix(plt[i, coordinate.cols])
    these.coord <- project(these.coord, proj = proj)
    spStats <- vector("list", length=length(d)) # numeric(length(d))
    for(j in 1:length(d)){
        p <- try(spatstat:::disc(d[j], these.coord), silent=TRUE)
        p <- as(p, 'SpatialPolygons')
        proj4string(p) <- CRS(proj4string(rast))
        ## masking is more time intensive on larger
        ## rasters so crop first
        new.rast <- crop(rast, extent(p))
        new.rast <- mask(new.rast, p)
        spStats[[j]] <- cellStats(new.rast, FUN)/(pi*d[j]^2)
    }
    names(spStats) <- d
    return(spStats)
}



calcPopCheck <- function(i,
                         d, ## buffer RADIUS
                         plt, ## plot csv
                         rast, ## raster to calculate stats from
                         ## projections from
                         FUN = sum, ## function for summing pop densities in buffers
                         ## with plot data
                         coordinate.cols = c("centerpoint_longitude", "centerpoint_latitude"),
                         ## crs =
                         ##     "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                         proj =
                           "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"){
  
  ## Create buffers of a given size, then calculate total population density in buffers
  ## rast <- projectRaster(rast, crs = crs, method = 'ngb')
  these.coord <- as.matrix(plt[i, coordinate.cols])
  these.coord <- project(these.coord, proj = proj)
  spStats <- vector("list", length=length(d)) # numeric(length(d))
  for(j in 1:length(d)){
    p <- try(spatstat:::disc(d[j], these.coord), silent=TRUE)
    p <- as(p, 'SpatialPolygons')
    proj4string(p) <- CRS(proj4string(rast))
    ## masking is more time intensive on larger
    ## rasters so crop first
    new.rast <- crop(rast, extent(p))
    new.rast <- mask(new.rast, p)
    spStats[[j]] <- cellStats(new.rast, FUN)
  }
  names(spStats) <- d
  return(spStats)
}