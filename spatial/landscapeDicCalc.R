rm(list=ls())
setwd('~/Dropbox/vitalSigns/analysis/vital_signs/spatial')
source('src/intialize.R')

## Test Function
## test.farm <- tanz.farm[2,]
## buff <- seq(10, 1100, by=100)
## testlapply <- lapply(1:13, calcSpStats, d = 100,
## plt = tanz.farm, plt.name = 'description', rast =  tanz)

buff <- seq(1000, 10000, by=1000)

## Tanzania Analysis
nplot <- nrow(tanz.farm)
spstats.mult.tanz <- lapply(1:nplot, calcSpStats, d = buff,
                            plt = tanz.farm, plt.name = 'description', rast =  tanz)
names(spstats.mult.tanz) <- tanz.farm$description
save(spstats.mult.tanz, file = file.path(save.path, 'tanz_stats.rdata'))

## Ghana Analysis
nplot <- nrow(gha.farm)
spstats.mult.gha <- lapply(1:nplot, calcSpStats, d = buff,
                           plt = gha.farm, plt.name = 'description', rast =  gha)
names(spstats.mult.gha) <- gha.farm$description
save(spstats.mult.gha, file = file.path(save.path, 'gha_stats.rdata'))

## Uganda Analysis
nplot <- nrow(ug.farm)
spstats.mult.ug <- lapply(1:nplot, calcSpStats, d = buff,
                          plt = ug.farm, plt.name = 'description', rast =  ug)
names(spstats.mult.ug) <- ug.farm$description
save(spstats.mult.ug, file = file.path(save.path, 'ug_stats.rdata'))
