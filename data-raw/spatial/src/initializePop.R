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
tanz <- readGDAL(system.file("extdata/spatial/tan_pop", 'TZA_popmap10_v2b.tif', package = "vitalSigns"))
tanz <- raster(tanz)

gha <- readGDAL(system.file("extdata/spatial/gha_pop", 'GHA10adj_040213.tif', package = "vitalSigns"))
gha <- raster(gha)

ug <- readGDAL(system.file("extdata/spatial/uga_pop", 'UGA_pph_v2b_2010_UNadj.tif', package = "vitalSigns"))
ug <- raster(ug)

rwa <- readGDAL(system.file("extdata/spatial/rwa_pop", 'RWA_pph_v2b_2010_UNadj.tif', package = "vitalSigns"))
rwa <- raster(rwa)


