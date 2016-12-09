library(dplyr)
library(reshape2)
library(ggplot2)

load("~/Documents/Berkeley/vital_signs/output/tanz_stats.rdata")
load("~/Documents/Berkeley/vital_signs/output/gha_stats.rdata")
load("~/Documents/Berkeley/vital_signs/output/ug_stats.rdata")


get_buffer_data <- function(data, buffer_size){
  bufdat <- data.frame()
  for (i in 1:length(data)){
    name <- names(data[i])
    clust <- as.data.frame(data[[i]][buffer_size])
    clust <- cbind(cluster = name, clust)
    bufdat <- rbind(bufdat, clust)
  }
  return(bufdat)
}

tanz <- get_buffer_data(spstats.mult.tanz, '810')
tanz_crop <- tanz %>% filter(X810.class == 30)

#Buffer of 810 m
tanz <- rbind(spstats.mult.tanz$`Sumbawanga Cluster`$`810`, spstats.mult.tanz$`Ihemi Cluster - Mufindi`$`810`)

sumb <- as.data.frame(spstats.tanz$`Sumbawanga Cluster`)
sumb <- cbind(cluster = 'Sumbawanga', sumb)
sumb2 <- as.data.frame(spstats.tanz$`Sumbawanga2 Cluster`)
sumb2 <- cbind(cluster = 'Sumbawanga2', sumb2)
ih <- as.data.frame(spstats.tanz$`Ihemi Cluster`)
ih <- cbind(cluster = 'Ihemi', ih)
ih2 <- as.data.frame(spstats.tanz$`Ihemi2 Cluster`)
ih2 <- cbind(cluster = 'Ihemi2', ih2)
lud <- as.data.frame(spstats.tanz$`Ludewa Cluster`)
lud <- cbind(cluster = 'Ludewa', lud)
kil <- as.data.frame(spstats.tanz$`Kilombero Cluster`)
kil <- cbind(cluster = 'Kilombero', kil)
mb <- as.data.frame(spstats.tanz$`MBarali Cluster`)
mb <- cbind(cluster = 'Mbarali', mb)
ruf <- as.data.frame(spstats.tanz$`Rufiji Cluster`)
ruf <- cbind(cluster = 'Rufiji', ruf)

giant <- rbind(sumb, ih, lud, ih2, kil, mb, sumb2, ruf)

irrigated.croplands <- giant %>% filter(X10.class == 11)
rain.croplands <- giant %>% filter(X10.class == 14)
mosaic.croplands <- giant %>% filter(X10.class == 20)

ggplot(data = melt(mosaic.croplands[,c("X10.total.area",
         "X10.patch.density", "X10.mean.shape.index")]),
       mapping = aes(x = value)) + geom_histogram(bins = 5) + facet_wrap(~variable, scales = 'free_x') + ggtitle("Mosaic Cropland")

ggplot(data = melt(irrigated.croplands[,c("X10.total.area",
         "X10.patch.density", "X10.mean.shape.index")]),
       mapping = aes(x = value)) + geom_histogram(bins = 5) + facet_wrap(~variable, scales = 'free_x') + ggtitle("Irrigated Cropland")

ggplot(data = melt(rain.croplands[,c("X10.total.area", "X10.patch.density", "X10.mean.shape.index")]), mapping = aes(x = value)) + geom_histogram(bins = 5) + facet_wrap(~variable, scales = 'free_x') + ggtitle("Rainfed Cropland")




##Mapping

# ## map shapefile
# library(RColorBrewer)
# colors <- brewer.pal(9, "BuGn")
# library(ggmap)
# mapImage <- get_map(location = c(lon = 35, lat = -6),
#                     color = "color",
#                     source = "osm",
#                                         # maptype = "terrain",
#                     zoom = 6)
# landscape <- fortify(tanz.farm)
# ggmap(mapImage) +
#   geom_polygon(aes(x = long,
#                    y = lat,
#                    group = group),
#                data = landscape,
#                color = colors[9],
#                fill = colors[6],
#                alpha = 0.5) +
#   labs(x = "Longitude",
#        y = "Latitude")

landscape <- fortify(tanz.farm)
gplot(tanz) + geom_tile(aes(fill = value)) +
  facet_wrap(~ variable) +
  scale_fill_gradient(low = 'white', high = 'blue') +
  coord_equal() +
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group),
               data = landscape,
               alpha = 0.5) +
  labs(x = "Longitude",
       y = "Latitude")

ggplot(data = melt(rain.croplands[,c("X10.total.area",
         "X10.patch.density", "X10.mean.shape.index")]),
       mapping = aes(x = value)) + geom_histogram(bins = 5) + facet_wrap(~variable, scales = 'free_x') + ggtitle("Rainfed Cropland")
