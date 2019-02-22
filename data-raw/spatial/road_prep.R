### Code to get roads for the 4 countries

### Load libraries
library(tidyverse)
library(sf)
library(raster)
library(rworldmap)

### Set paths

### Read in road data
# roads <- readOGR(dsn = "C:/Users/Katherine Siegel/Dropbox/vitalSigns/data/spatial/roads/gROADS-v1-africa-shp",
#                  layer = "gROADS-v1-africa")

#### Save road data to intermediate (int) data folder
## save(roads, "data/int/roads.Rdata"))

### Load road data - we did this because the shp of African roads took so long to load
load("data/int/roads.Rdata")
roads <- st_as_sf(roads)

### Get country maps from rworldmaps-- used this to crop the roads data 
data(countriesLow)
country_map <- countriesLow

### Subset Vital Signs countries
vs_countries <- country_map[country_map@data$ISO3 %in% c("RWA",
                                                         "TZA", "UGA",
                                                         "GHA"),] %>% st_as_sf()

### Put vs_countries in same projection as roads
vs_countries <-  st_transform(vs_countries, crs(roads))

### Extract roads in VS countries
vs_roads <- st_intersection(roads, vs_countries)

### Write sf of roads
save(vs_roads, file = "data/int/vs_roads.rdata")