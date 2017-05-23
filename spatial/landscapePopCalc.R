rm(list=ls())
setwd("~/Dropbox/vitalSigns/analysis/vital_signs/spatial")
source('src/initializePop.R')

buff <- seq(1000, 3000, by=1000)
crs <-  "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

## Tanzania Analysis
nplot <- nrow(tanz.farm)
tanz <- projectRaster(tanz, crs = crs, method = 'ngb')
popstats.mult.tanz <- lapply(1:nplot, calcPopStats, d = buff,
                            plt = tanz.farm,
                            rast =  tanz)
names(popstats.mult.tanz) <- tanz.farm$landscape_no


save(popstats.mult.tanz,
     file = file.path(save.path, 'tanz_popstats.rdata'))

## Ghana Analysis
nplot.gha <- nrow(gha.farm)
gha <- projectRaster(gha, crs = crs, method = 'ngb')
popstats.mult.gha <- lapply(1:nplot.gha, calcPopStats, d = buff,
                            plt = gha.farm,
                            rast =  gha)
names(popstats.mult.gha) <- gha.farm$landscape_no

save(popstats.mult.gha,
     file = file.path(save.path, 'gha_popstats.rdata'))

## Uganda Analysis
nplot.ug <- nrow(ug.farm)
ug <- projectRaster(ug, crs = crs, method = 'ngb')
popstats.mult.ug <- lapply(1:nplot.ug, calcPopStats, d = buff,
                          plt = ug.farm,
                          rast =  ug)
names(popstats.mult.ug) <- ug.farm$landscape_no

save(popstats.mult.ug,
     file = file.path(save.path, 'ug_popstats.rdata'))

## Rwanda Analysis
nplot.rwa <- nrow(rwa.farm)
rwa <- projectRaster(rwa, crs = crs, method = 'ngb')
popstats.mult.rwa <- lapply(1:nplot.rwa, calcPopStats, d = buff,
                            plt = rwa.farm,
                            rast = rwa)
names(popstats.mult.rwa) <- rwa.farm$landscape_no

save(popstats.mult.rwa,
     file = file.path(save.path, 'rwa_popstats.rdata'))



