library(sp)
library(raster)
library(spatstat)
library(maptools)
library(SDMTools)
library(rgdal)
source('src/CalcPopStats.R')

data.path <- '~/Documents/pop_density/'
save.path <- '~/Dropbox/vitalSigns/saved/spatial/'

## Read in landscape data
farm <- na.omit(read.csv(system.file("extdata/spatial", 'landscape.csv', package = "vitalSigns")))
tanz.farm <- farm[farm$country == "TZA",]
gha.farm <- farm[farm$country == "GHA",]
ug.farm <- farm[farm$country == "UGA",]
rwa.farm <- farm[farm$country == "RWA",]

## Read in population density data for all countries (2010, UN adjusted)
tanz <- readGDAL(file.path(data.path,'TZA-POP/TZA_popmap10_v2b.tif'))
tanz <- raster(tanz)

gha <- readGDAL(file.path(data.path,'GHA-POP/GHA10adj_040213.tif'))
gha <- raster(gha)

ug <- readGDAL(file.path(data.path,'UGA-POP/UGA_pph_v2b_2010_UNadj.tif'))
ug <- raster(ug)

rwa <- readGDAL(file.path(data.path,'RWA-POP/RWA_pph_v2b_2010_UNadj.tif'))
rwa <- raster(rwa)


