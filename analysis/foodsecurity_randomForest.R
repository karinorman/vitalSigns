library(randomForest)
library(RColorBrewer)
library(forestFloor)
library(here)

source(here("analysis", "src", "runMods.R"))

load(here("data", "ag.rda"))
ag <- ag[ag$Round == 1,]

keep.RWA <- "wRWA"

if(keep.RWA == "woRWA"){
  ag <- ag[ag$Country != "RWA",]
}

# ys <- c("long_rainy_crop_value_area", "short_rainy_crop_value_area",
#         "long_rainy_yield_area", "short_rainy_yield_area")

ys <- c("long_rainy_yield_area", "short_rainy_yield_area")

xvars <- c("tenure",
           "Country",
           "total_paid_wages",
           "extension", "prop_pot_labor", "Landscape..",
           "org_fert",
           "inorg_fert",
           "pest_herb",
           "total_div",
           "long_rainy_org_fert_type",
           "long_rainy_inorg_fert_type",
           "short_rainy_org_fert_type",
           "short_rainy_inorg_fert_type",
           "long_rainy_pest_herb_type",
           "short_rainy_pest_herb_type",
           "long_rainy_eros_type",
           ## "short_rainy_eros_type",
           "livestock_int",
           "crop_rotation",
           "intercrop", "total_crop_rich",
           "crop_div",
           ## "simpson.div",
           "prop_education", "profit",
           "long_rainy_irrigation",
           ## "long_rainy_irrigation_type",
           "water_source_rainy",
           "water_source_dry",
           "long_rainy_soil_quality",
           "pop_density")


formulas.tree <-lapply(ys, function(x) {
  as.formula(paste(x, "~", paste(paste(xvars, collapse="+"))))
})

ag.sub <- ag[!apply(ag[,ys], 1, function(x) any(is.na(x))),]

## fill in NAs using tree
filled.data <- lapply(formulas.tree,  rfImpute,
                      data=ag.sub,
                      ntree=1000,
                      niter=10)

## training and testing data
dev.val.data <- lapply(filled.data, sepDevValData)
ag.dev <- lapply(dev.val.data, function(x) x$dev)
ag.val <- lapply(dev.val.data, function(x) x$val)

practice.forest <- mapply(function(a, b)
  randomForest(formula= a,
               data= b,
               importance=TRUE,
               sampsize=25,
               ntree=1000,
               mtry=4,
               keep.inbag = TRUE,
               keep.forest = TRUE,
               proximity=TRUE),
  a=formulas.tree,
  b=ag.dev,
  SIMPLIFY=FALSE)

## Predict Response Variable Value using Random Forest
f <- function(){
  plot(ff, col=Col, plot_GOF = TRUE, plot_seq = 1:length(xvars),
       orderByImportance=TRUE)
}

val.forests <- vector(mode="list", length=length(practice.forest))


for(i in 1:length(practice.forest)){
  val.forests[[i]] <- checkRandomForest(dev.data=ag.dev[[i]],
                                        val.data=ag.val[[i]],
                                        rf=practice.forest[[i]],
                                        plot.main=ys[i], y=ys[i])
  ff <- forestFloor(practice.forest[[i]],
                    ag.dev[[i]],
                    calc_np=TRUE)
  Col <- fcol(ff, cols=1, outlier.lim = 2.5)
  pdf.f(f, file.path(sprintf("figures/yield_practices/%s/%s.pdf",
                             keep.RWA, ys[i])),
        height=11)
}


###### OLD CODE
# food.insec.forest <- randomForest(insec.form,
#                                   data=ag.dev.insec,
#                                   importance=TRUE,
#                                   sampsize=25,
#                                   ntree=5000,
#                                   mtry=4,
#                                   keep.inbag = TRUE,
#                                   keep.forest = TRUE,
#                                   proximity=TRUE
#                                   ## na.action=na.roughfix
#                                   )
# 
# 
# food.insec <- checkRandomForest(dev.data=ag.dev.insec,
#                                 val.data=ag.val.insec,
#                                 rf=food.insec.forest,
#                                 plot.main="Food insecurity",
#                                 y=ys)
# 
# ## *****************************************************************
# library(forestFloor)
# ff <- forestFloor(food.insec.forest,
#                   ag.dev.insec,
#                   calc_np=TRUE)
# Col <- fcol(ff, cols=1, outlier.lim = 2.5)
# 
# f <- function(){
#     plot(ff, col=Col, plot_GOF = TRUE, plot_seq = 1:15,
#          orderByImportance=TRUE)
# }
# 
# pdf.f(f, file.path("figures/woRWA", sprintf("%s.pdf", "ff")),
#       height=11)
# 
# ## show3d(ff, col=Col, plot_GOF=FALSE)
# ## *****************************************************************
# 
# 
# plotRF <- function(this.pred, data, predictor, response,
#                    ylims=c(0,1), pretty.breaks, cols, first.levs, first.pred, ...){
#     these.data <- data[data[, predictor] == this.pred,]
#     pretty.breaks <- pretty(range(data[, response], na.rm=TRUE), min.n=2)
#     plot(NULL, ylim=ylims, xlim=range(pretty.breaks),
#          xlab="", ylab="", bty="n", ...)
#     ## legend("topleft", legend= first.levs, col=cols, bty="n", pch=15)
#     for(lev in first.levs){
#         ## d.y <- try(density(these.data[, response][these.data[, first.pred] == lev],
#         ##                    na.rm=TRUE), silent=TRUE)
#         ## if(inherits(d.y, "try-error")) next
#         ## ## points(y=d.y$y, x=d.y$x, type='l', ltd=5,
#         ## ##        col=cols[lev])
#         ## polygon(d.y,
#         ##         col=cols[lev])
# 
#         d.y <- try(hist(these.data[, response][these.data[, first.pred] == lev],
#                         add=TRUE, freq=FALSE, breaks=pretty.breaks, col=cols[lev]), silent=TRUE)
# 
#         ## d.y <- try(hist(these.data[, response][these.data[, first.pred] == lev],
#         ##                 plot=FALSE, frequency=FALSE, breaks=pretty.breaks), silent=TRUE)
#         if(inherits(d.y, "try-error")) next
#         ## barplot(rev(d.y$density),rev(d.y$breaks),horiz=TRUE, space=0,
#         ##         xlim=xlims, col=cols[lev], add=TRUE)
#     }
#     text((max(pretty.breaks)/2), par("usr")[3] - 0.1, srt = 45, adj = 1,
#          labels = this.pred, xpd = TRUE, cex=1.5)
# 
# 
# }
# 
# important.varibles <- names(sort(ff$importance, decreasing=TRUE)[2:15])
# 
# 
# plotVars <- function(){
#     layout(matrix(1:length(levels(ag[,k])), nrow=1))
#     par(oma=c(1,2,1,1),
#         mar=c(10,2,0,0))
#     first.levs <- levels(ag[, "Country"])
#     cols <- rainbow(length(first.levs), alpha=0.5)
#     names(cols) <- first.levs
#     for(lev in levels(ag[,k])){
#         plotRF(lev,
#                ag,  predictor=k,
#                response="food_insecurity",
#                ## yaxt="n",
#                ylims=c(0,1),
#                cols=cols,
#                first.levs=first.levs,
#                first.pred="Country",
#                pretty.breaks = pretty(range(ag$food_insecurity,
#                                             na.rm=TRUE)))
#         legend("topleft", legend= first.levs, col=cols, bty="n", pch=15)
# 
#     }
# }
# 
# for(k in important.varibles){
#     pdf.f(plotVars, file.path("figures/woRWA", sprintf("%s.pdf", k)),
#           width=30)
# }
# 
# 
# 
# boxplot(ag.dev.insec$food_insecurity ~ ag.dev.insec$Country)
# ## boxplot(ag.dev.insec$food_insecurity ~ ag.dev.insec$Landscape..)
# 
# ## boxplot(ag.dev.insec$food_insecurity ~ ag.dev.insec$water_source_dry)
# ## boxplot(ag.dev.insec$food_insecurity ~ ag.dev.insec$water_source_rainy)
# 
# ## boxplot(ag.dev.insec$food_insecurity ~ ag.dev.insec$long_rainy_inorg_fert_type)
# ## boxplot(ag.dev.insec$food_insecurity ~ ag.dev.insec$short_rainy_inorg_fert_type)
# 
# ## boxplot(ag.dev.insec$food_insecurity ~
# ## ag.dev.insec$short_rainy_pest_herb_type)
# 
# 
# ## boxplot(ag.dev.insec$food_insecurity ~
# ##             ag.dev.insec$long_rainy_org_fert_type)
# 
# ## boxplot(ag.dev.insec$food_insecurity ~
# ##             ag.dev.insec$long_rainy_irrigation)
# 
# 
# ## boxplot(ag.dev.insec$food_insecurity ~ ag.dev.insec$long_rainy_pest_herb_type)
# ## boxplot(ag.dev.insec$food_insecurity ~ ag.dev.insec$org_fert)
# 
# ## plot(ag.dev.insec$food_insecurity ~ ag.dev.insec$total_div)
# ## boxplot(ag.dev.insec$food_insecurity ~ ag.dev.insec$short_rainy_org_fert_type)
# 
# 
# ## rwanda has higher food insecutiry over all
# ## subsidized water vending station, unprotected well with pump and
# ## protected well without pump all have high insecurity
# ## farmers that use DAP (look like they are mostly from Rowanda)
