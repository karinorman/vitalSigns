library(sp)
library(sf)
library(raster)
library(spatstat)
library(maptools)
library(SDMTools)
library(rgdal)
library(data.table)
source('/data-raw/spatial/src/initializeRds.R')

buff <- seq(1000, 3000, by=1000)
crs <-  "+proj=utm +zone=37 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
crs_vs <- crs(vs_countries)

### Open roads
load("~/Dropbox/vitalSigns/saved/spatial/extracted_roads.Rdata")
## roads_in_vs is a matrix (long x lat)

#### add column to matrix so can use rasterFromXYZ
z <- as.vector(rep(1, len = nrow(roads_in_vs)))
roads_in_vs <- cbind(roads_in_vs, z)
colnames(roads_in_vs) <- c("x", "y", "layer")
roads_in_vs <- as.data.frame(as.table(roads_in_vs)) ## not enough space to do this

#### convert matrix to raster
#### rd <- rasterFromXYZ(roads_in_vs, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
rd <- rasterFromXYZ(roads_in_vs, crs = crs_vs)


#### Tanzania Analysis
nplot <- nrow(tanz.farm)
roadstats.mult.tanz <- lapply(1:nplot, calcRdsStats, d = buff,
                            plt = tanz.farm,
                            rast =  vs_rd_coord)
names(roadstats.mult.tanz) <- tanz.farm$landscape_no


save(roadstats.mult.tanz,
     file = file.path(save.path, 'tanz_roadstats.rdata'))

#### Ghana Analysis
nplot.gha <- nrow(gha.farm)
roadstats.mult.gha <- lapply(1:nplot.gha, calcRdsStats, d = buff,
                            plt = gha.farm,
                            rast =  vs_rd_coord)
names(roadstats.mult.gha) <- gha.farm$landscape_no

save(roadstats.mult.gha,
     file = file.path(save.path, 'gha_roadstats.rdata'))

#### Uganda Analysis
nplot.ug <- nrow(ug.farm)
roadstats.mult.ug <- lapply(1:nplot.ug, calcRdsStats, d = buff,
                          plt = ug.farm,
                          rast =  vs_rd_coord)
names(roadstats.mult.ug) <- ug.farm$landscape_no

save(roadstats.mult.ug,
     file = file.path(save.path, 'ug_roadstats.rdata'))

#### Rwanda Analysis
nplot.rwa <- nrow(rwa.farm)
roadstats.mult.rwa <- lapply(1:nplot.rwa, calcRdsStats, d = buff,
                            plt = rwa.farm,
                            rast = vs_rd_coord)
names(roadstats.mult.rwa) <- rwa.farm$landscape_no

save(roadstats.mult.rwa,
     file = file.path(save.path, 'rwa_roadstats.rdata'))

#### incomplete code



