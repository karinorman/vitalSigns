library(sp)
library(raster)
library(spatstat)
library(maptools)
library(SDMTools)
library(rgdal)
library(data.table)
source('src/initialize.R')

buff <- seq(1000, 3000, by=1000)
crs <-  "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

## Tanzania Analysis
nplot <- nrow(tanz.farm)
tanz <- projectRaster(tanz, crs = crs, method = 'ngb')

spstats.mult.tanz <- lapply(1:nplot, calcSpStats, d = buff,
                            plt = tanz.farm,
                            rast =  tanz)
names(spstats.mult.tanz) <- tanz.farm$landscape_no
sum.tanz <- lapply(spstats.mult.tanz, calcLandStats)

save(spstats.mult.tanz, sum.tanz,
     file = 'data/int/tanz_stats.rdata')

## Ghana Analysis
nplot.gha <- nrow(gha.farm)
gha <- projectRaster(gha, crs = crs, method = 'ngb')
spstats.mult.gha <- lapply(1:nplot.gha, calcSpStats, d = buff,
                            plt = gha.farm,
                            rast =  gha)

names(spstats.mult.gha) <- gha.farm$landscape_no

sum.gha <- lapply(spstats.mult.gha, calcLandStats)

save(spstats.mult.gha, sum.gha,
     file = 'data/int/gha_stats.rdata')

## Uganda Analysis
nplot.ug <- nrow(ug.farm)
ug <- projectRaster(ug, crs = crs, method = 'ngb')
spstats.mult.ug <- lapply(1:nplot.ug, calcSpStats, d = buff,
                          plt = ug.farm,
                          rast =  ug)
names(spstats.mult.ug) <- ug.farm$landscape_no

sum.ug <- lapply(spstats.mult.ug, calcLandStats)

save(spstats.mult.ug, sum.ug,
     file = 'data/int/ug_stats.rdata')

###### Combine Country Stats ##########

get_buffer_data <- function(data, buffer_size){
  bufdat <- data.frame()
  for (i in 1:length(data)){
    name <- names(data[i])
    clust <- as.data.frame(data[[i]][buffer_size])
    clust <- cbind(cluster = name, clust)
    bufdat <- rbind(bufdat, clust)
  }
  return(bufdat)
}

## Calc diversity metric
landStats <- function(class.stats){
  simpson <- function(stats){
    1- sum(stats[,'prop.landscape']^2)
  }
  land.stats <- lapply(class.stats, function(x){
    if(!is.null(x)){
      means <- apply(x, 2, mean, na.rm=TRUE)
      simpson.div <- simpson(x)
      names(simpson.div) <- 'simpson.div'
      return(c(means, simpson.div))
    } else {
      return(NA)
    }
  })
  return(land.stats)
}

## Get stats in pretty dataframe preserving landscape and buffer labesl
unlist_landStats <- function(land.stats){
  div.clean <- data.frame()
  for(i in 1:length(land.stats)){
    clust <- as.data.frame(do.call(rbind, land.stats[[i]]))
    setDT(clust, keep.rownames = TRUE)[]
    clust <- cbind(cluster = names(land.stats[i]), clust)
    div.clean <- rbind(div.clean, clust)
  }
  names(div.clean)[[2]] <- 'buffer'
  return(div.clean)
}

div.tanz <- lapply(spstats.mult.tanz, landStats)
div.tanz <- unlist_landStats(div.tanz)
div.tanz <- cbind(country = 'Tanzania', div.tanz)

div.gha <- lapply(spstats.mult.gha, landStats)
div.gha <- unlist_landStats(div.gha)
div.gha <- cbind(country = 'Ghana', div.gha)

div.ug <- lapply(spstats.mult.ug, landStats)
div.ug <- unlist_landStats(div.ug)
div.ug <- cbind(country = 'Uganda', div.ug)

landscape_diversity <- rbind(div.tanz, div.gha, div.ug)

usethis::use_data(landscape_diversity)



