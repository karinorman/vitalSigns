### Load libraries
library(tidyverse)
library(sf)

### Set wd and paths
setwd("~/Dropbox/vitalSigns/analysis/vital_signs/spatial")
## source('src/CalcRdsStats.R')
data.path <- '~/Dropbox/vitalSigns/data/spatial/'
save.path <- '~/Dropbox/vitalSigns/saved/spatial/'

### Read in geo data for landscapes -- this is the data that the roads will get linked to for the road stats
farm <- na.omit(read.csv(file.path(data.path,'landscape.csv')))
tanz.farm <- farm[farm$country == "TZA",]
gha.farm <- farm[farm$country == "GHA",]
ug.farm <- farm[farm$country == "UGA",]
rwa.farm <- farm[farm$country == "RWA",]

### Read in road data from road_prep.R
roads <- st_read(file.path(data.path, "vs_roads.shp"))




