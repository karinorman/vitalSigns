library(sp)
library(raster)
library(spatstat)
library(maptools)
library(SDMTools)
library(rgdal)
source('src/CalcSpStats.R')

## Read in data
farm <- na.omit(read.csv(system.file("extdata/spatial", 'landscape.csv', package = "vitalSigns")))
tanz.farm <- farm[farm$country == "TZA",]
gha.farm <- farm[farm$country == "GHA",]
ug.farm <- farm[farm$country == "UGA",]

tanz <- readGDAL(system.file("extdata/spatial/tanz", 'tz_tm1-57palglobdem_landcover.dat', package = "vitalSigns"))
tanz <- raster(tanz)

gha <- readGDAL(system.file('extdata/spatial/ghana', 'gh_tm1-57palglobdem_landcover.dat', package = "vitalSigns"))
gha <- raster(gha)

ug <- readGDAL(system.file('extdata/spatial/uganda', 'ug_tm1-57palglobdem_landcover.dat', package = "vitalSigns"))
ug <- raster(ug)
