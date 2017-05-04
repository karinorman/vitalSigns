rm(list=ls())
setwd('~/Dropbox/vitalSigns/analysis/vital_signs/spatial')
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
     file = file.path(save.path, 'tanz_stats.rdata'))

## Ghana Analysis
nplot.gha <- nrow(gha.farm)
gha <- projectRaster(gha, crs = crs, method = 'ngb')
spstats.mult.gha <- lapply(1:nplot.gha, calcSpStats, d = buff,
                            plt = gha.farm,
                            rast =  gha)

names(spstats.mult.gha) <- gha.farm$landscape_no

sum.gha <- lapply(spstats.mult.gha, calcLandStats)
save(spstats.mult.gha, sum.gha,
     file = file.path(save.path, 'gha_stats.rdata'))

## Uganda Analysis
nplot.ug <- nrow(ug.farm)
ug <- projectRaster(ug, crs = crs, method = 'ngb')
spstats.mult.ug <- lapply(1:nplot.ug, calcSpStats, d = buff,
                          plt = ug.farm,
                          rast =  ug)
names(spstats.mult.ug) <- ug.farm$landscape_no

sum.ug <- lapply(spstats.mult.ug, calcLandStats)
save(spstats.mult.ug, sum.ug,
     file = file.path(save.path, 'ug_stats.rdata'))


