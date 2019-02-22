source('src/initializePop.R')

buff <- seq(5000, 20000, by=1500)
crs <-  "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

## Tanzania Analysis
nplot <- nrow(tanz.farm)
tanz <- projectRaster(tanz, crs = crs, method = 'ngb')
popstats.mult.tanz <- lapply(1:nplot, calcPopStats, d = buff,
                            plt = tanz.farm,
                            rast =  tanz)
names(popstats.mult.tanz) <- tanz.farm$landscape_no

save(popstats.mult.tanz,
     file = 'data/int/tanz_popstats.rdata')

## Ghana Analysis
nplot.gha <- nrow(gha.farm)
gha <- projectRaster(gha, crs = crs, method = 'ngb')
popstats.mult.gha <- lapply(1:nplot.gha, calcPopStats, d = buff,
                            plt = gha.farm,
                            rast =  gha)
names(popstats.mult.gha) <- gha.farm$landscape_no

save(popstats.mult.gha,
     file = 'data/int/gha_popstats.rdata')

## Uganda Analysis
nplot.ug <- nrow(ug.farm)
ug <- projectRaster(ug, crs = crs, method = 'ngb')
popstats.mult.ug <- lapply(1:nplot.ug, calcPopStats, d = buff,
                          plt = ug.farm,
                          rast =  ug)
names(popstats.mult.ug) <- ug.farm$landscape_no

save(popstats.mult.ug,
     file = 'data/int/ug_popstats.rdata')

## Rwanda Analysis
nplot.rwa <- nrow(rwa.farm)
rwa <- projectRaster(rwa, crs = crs, method = 'ngb')
popstats.mult.rwa <- lapply(1:nplot.rwa, calcPopStats, d = buff,
                            plt = rwa.farm,
                            rast = rwa)
names(popstats.mult.rwa) <- rwa.farm$landscape_no

save(popstats.mult.rwa,
     file = 'data/int/rwa_popstats.rdata')



#### Check population sizes
### Check TZA
# nplot <- nrow(tanz.farm)
# check.tanz <- lapply(1:nplot, calcPopCheck, d = buff,
#                              plt = tanz.farm,
#                              rast =  tanz)
# names(check.tanz) <- tanz.farm$landscape_no
# tza.check <- t(as.data.frame(check.tanz, stringsAsFactors = FALSE))
# tza.check <- as.data.frame(tza.check, stringsAsFactors = FALSE)
# tza.check$Landscape <- lapply(strsplit(rownames(tza.check), split="\\."), function(x) x[1])
# tza.check$Buffer_radius <- lapply(strsplit(rownames(tza.check), split="\\."), function(x) x[2])
# names(tza.check)[names(tza.check) == 'V1'] <- 'Population'
# rownames(tza.check) <- NULL
# tza.check$Landscape <- as.character(tza.check$Landscape)
# tza.check$Buffer_area <- pi * (as.numeric(tza.check$Buffer_radius) ^2)
# tza.check$Buffer_radius <- as.numeric(tza.check$Buffer_radius)
# tza.check <- tza.check[c(2, 3, 4, 1)]
# tza.check$Pop_density_km2 <- as.numeric(tza.check$Population/tza.check$Buffer_area/0.000001)
# write.csv(tza.check, file = file.path(save.path, 'tza_population.csv'))

### Check Ghana
# nplot <- nrow(gha.farm)
# check.gha <- lapply(1:nplot, calcPopCheck, d = buff,
#                      plt = gha.farm,
#                      rast =  gha)
# names(check.gha) <- gha.farm$landscape_no
# gha.check <- t(as.data.frame(check.gha, stringsAsFactors = FALSE))
# gha.check <- as.data.frame(gha.check, stringsAsFactors = FALSE)
# gha.check$Landscape <- lapply(strsplit(rownames(gha.check), split="\\."), function(x) x[1])
# gha.check$Buffer_radius <- lapply(strsplit(rownames(gha.check), split="\\."), function(x) x[2])
# names(gha.check)[names(gha.check) == 'V1'] <- 'Population'
# rownames(gha.check) <- NULL
# gha.check$Landscape <- as.character(gha.check$Landscape)
# gha.check$Buffer_area <- pi * (as.numeric(gha.check$Buffer_radius) ^2)
# gha.check$Buffer_radius <- as.numeric(gha.check$Buffer_radius)
# gha.check <- gha.check[c(2, 3, 4, 1)]
# gha.check$Pop_density_km2 <- as.numeric(gha.check$Population/gha.check$Buffer_area/0.000001)
# write.csv(gha.check, file = file.path(save.path, 'gha_population.csv'))
