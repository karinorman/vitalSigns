library(dplyr)
library(reshape2)
library(ggplot2)

load("~/Documents/Berkeley/vital_signs/output/tanz_stats.rdata")

load("~/Documents/Berkeley/vital_signs/output/tanz_stats.rdata")

tanz.names <- c("Sumbawanga Cluster", "Ihemi Cluster", "Ludewa Cluster", "Ihemi2 Cluster", "Kilombero Cluster", "MBarali Cluster", "Sumbawanga2 Cluster", "Rufiji Cluster")
names(spstats.tanz) <- tanz.names
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

ggplot(data = melt(rain.croplands[,c("X10.total.area",
         "X10.patch.density", "X10.mean.shape.index")]),
       mapping = aes(x = value)) + geom_histogram(bins = 5) + facet_wrap(~variable, scales = 'free_x') + ggtitle("Rainfed Cropland")
