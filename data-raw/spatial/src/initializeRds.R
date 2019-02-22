### Load libraries
library(tidyverse)
library(sf)
source('src/CalcRdsStats.R')

### Read in geo data for landscapes -- this is the data that the roads will get linked to for the road stats
farm <- na.omit(read.csv(system.file("extdata/spatial", 'landscape.csv', package = "vitalSigns")))
tanz.farm <- farm[farm$country == "TZA",]
gha.farm <- farm[farm$country == "GHA",]
ug.farm <- farm[farm$country == "UGA",]
rwa.farm <- farm[farm$country == "RWA",]

### Read in road data from road_prep.R
roads <- load("data/int/vs_roads.shp")




