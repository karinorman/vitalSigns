rm(list=ls())
setwd("~/Dropbox/vitalSigns/analysis/vital_signs/spatial")
ncores <- 12
source('src/initializeRds.R')

buff <- seq(1000, 3000, by=1000)
crs <-  "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

## Tanzania Analysis
nplot <- nrow(tanz.farm)
tanz <- projectRaster(tanz, crs = crs, method = 'ngb')
rdstats.mult.tanz <- lapply(1:nplot, calcRdsStats, d = buff,
                            plt = tanz.farm,
                            rast =  tanz)
names(rdstats.mult.tanz) <- tanz.farm$landscape_no


save(rdstats.mult.tanz,
     file = file.path(save.path, 'tanz_rdstats.rdata'))

## Ghana Analysis
nplot.gha <- nrow(gha.farm)
gha <- projectRaster(gha, crs = crs, method = 'ngb')
rdstats.mult.gha <- lapply(1:nplot.gha, calcRdsStats, d = buff,
                            plt = gha.farm,
                            rast =  gha)
names(rdstats.mult.gha) <- gha.farm$landscape_no

save(rdstats.mult.gha,
     file = file.path(save.path, 'gha_rdstats.rdata'))

## Uganda Analysis
nplot.ug <- nrow(ug.farm)
ug <- projectRaster(ug, crs = crs, method = 'ngb')
rdstats.mult.ug <- lapply(1:nplot.ug, calcRdsStats, d = buff,
                          plt = ug.farm,
                          rast =  ug)
names(rdstats.mult.ug) <- ug.farm$landscape_no

save(rdstats.mult.ug,
     file = file.path(save.path, 'ug_rdstats.rdata'))

## Rwanda Analysis
nplot.rwa <- nrow(rwa.farm)
rwa <- projectRaster(rwa, crs = crs, method = 'ngb')
rdstats.mult.rwa <- lapply(1:nplot.rwa, calcRdsStats, d = buff,
                            plt = rwa.farm,
                            rast = rwa)
names(rdstats.mult.rwa) <- rwa.farm$landscape_no

save(rdstats.mult.rwa,
     file = file.path(save.path, 'rwa_rdstats.rdata'))



