library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(data.table)

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

#Buffer of 810 m
tanz <- get_buffer_data(spstats.mult.tanz, '810')
tanz_crop <- tanz %>% filter(X810.class == 30)
tanz <- rbind(spstats.mult.tanz$`Sumbawanga Cluster`$`810`, spstats.mult.tanz$`Ihemi Cluster - Mufindi`$`810`)

#Calc diversity metric
landStats <- function(class.stats){
  simpson <- function(stats){
    1- sum(stats[,"prop.landscape"]^2)
  }
  land.stats <- lapply(class.stats, function(x){
    if(!is.null(x)){
    means <- apply(x, 2, mean, na.rm=TRUE)
    simpson.div <- simpson(x)
    names(simpson.div) <- "simpson.div"
    return(c(means, simpson.div))
    } else {
      return(NA)
    }
  })
  return(land.stats)
}

#Get stats in pretty dataframe preserving landscape and buffer labesl
unlist_landStats <- function(land.stats){
  div.clean <- data.frame()
  for(i in 1:length(land.stats)){
    clust <- as.data.frame(do.call(rbind, land.stats[[i]]))
    setDT(clust, keep.rownames = TRUE)[]
    clust <- cbind(cluster = names(land.stats[i]), clust)
    div.clean <- rbind(div.clean, clust)
  }
  names(div.clean)[[2]] <- "buffer"
  return(div.clean)
}

div.tanz <- lapply(spstats.mult.tanz, landStats)
div.tanz <- unlist_landStats(div.tanz)
div.tanz <- cbind(country = "Tanzania", div.tanz)

div.gha <- lapply(spstats.mult.gha, landStats)
div.gha <- unlist_landStats(div.gha)
div.gha <- cbind(country = "Ghana", div.gha)

div.ug <- lapply(spstats.mult.ug, landStats)
div.ug <- unlist_landStats(div.ug)
div.ug <- cbind(country = "Uganda", div.ug)

simpson <- rbind(div.tanz[,c("country", "buffer", "simpson.div")], div.gha[,c("country", "buffer", "simpson.div")], div.ug[,c("country", "buffer", "simpson.div")])

get_density_plt <- function(data, var, buff, ...){
  data_buff <- data %>% filter(buffer == buff)
  dens <- density(unlist(data_buff[var]))
  plot(dens, ...)
}

###Histograms
#pdf("simpson_density.pdf", width=5.05, height = 5.05)
buff <- seq(1000, 10000, by=1000)
op <- par(mfrow = c(length(buff),3),
          oma = c(2,3,2,2),
          mar = c(.75,.75,.75,.75))
for (i in 1:length(buff)){
  if (i != length(buff)){
    get_density_plt(div.tanz, "simpson.div", buff[i], ylim=c(0,4), xlim=c(-.4,1), main = "", xaxt = 'n', col = 51)
    axis(side=1, labels = F)
  } else{
    get_density_plt(div.tanz, "simpson.div", buff[i], ylim=c(0,4), xlim=c(-.4,1), main = "", col = 51)
  }
  if (i != length(buff)){
    get_density_plt(div.ug, "simpson.div", buff[i], ylim=c(0,4), xlim=c(-.4,1), main = "", xaxt='n', yaxt='n', col = 132)
    axis(side=1, labels = F)
    axis(side=2, labels = F)
  } else{
    get_density_plt(div.ug, "simpson.div", buff[i], ylim=c(0,4), xlim=c(-.4,1), main = "", yaxt = 'n', col = 132)
    axis(side=2, labels = F)
  }
  if (i != length(buff)){
    get_density_plt(div.gha, "simpson.div", buff[i], ylim=c(0,4), xlim=c(-0.4, 1), main = "", xaxt = 'n', yaxt = 'n', col = 37)
    axis(side=1, labels = F)
    axis(side=2, labels = F)
  } else{
    get_density_plt(div.gha, "simpson.div", buff[i], ylim=c(0,4), xlim=c(-0.4, 1), main = "", yaxt = 'n', col = 37)
    axis(side=2, labels = F)
  }
}
par(op)
#dev.off()

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

# ## Map Raster
# landscape <- fortify(tanz.farm)
# gplot(tanz) + geom_tile(aes(fill = value)) +
#   facet_wrap(~ variable) +
#   scale_fill_gradient(low = 'white', high = 'blue') +
#   coord_equal() +
#   geom_polygon(aes(x = long,
#                    y = lat,
#                    group = group),
#                data = landscape,
#                alpha = 0.5) +
#   labs(x = "Longitude",
#        y = "Latitude")
# 
# ggplot(data = melt(rain.croplands[,c("X10.total.area",
#          "X10.patch.density", "X10.mean.shape.index")]),
#        mapping = aes(x = value)) + geom_histogram(bins = 5) + facet_wrap(~variable, scales = 'free_x') + ggtitle("Rainfed Cropland")
