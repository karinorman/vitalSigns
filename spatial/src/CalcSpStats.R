
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

calcSpStats <- function(i,
                        d, ## buffer RADIS
                        plt, ## plot csv
                        rast, ## raster to calculate stats from
                        ## projections from
                        FUN=ClassStat, ## function for calculating stats on buffers
                        ## with plot data
                        coordinate.cols = c("centerpoint_longitude", "centerpoint_latitude"),
                        ## crs =
                        ##     "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs",
                        proj =
                            "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"){

    ##   ClassStat is a function from the SDMTools package that calculates
    ## the frag stat metrics
    ## creates buffers of a given size, calculates frag stat metrics
    ## rast <- projectRaster(rast, crs = crs, method = 'ngb')
    these.coord <- as.matrix(plt[i, coordinate.cols])
    these.coord <- project(these.coord, proj = proj)
    spStats <- vector("list", length=length(d))
    for(j in 1:length(d)){
        p <- try(spatstat:::disc(d[j], these.coord), silent=TRUE)
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

calcLandStats <- function(class.stats){
    simpson <- function(stats){
        1- sum(stats[,"prop.landscape"]^2)
    }
    land.stats <- lapply(class.stats, function(x){
        means <- apply(x, 2, mean, na.rm=TRUE)
        simpson.div <- simpson(x)
        names(simpson.div) <- "simpson.div"
        return(c(means, simpson.div))
    })
    return(land.stats)
}
