rm(list=ls())
library(randomForest)
setwd("~/Dropbox/vitalSigns/analysis/vital_signs/survey")
save.dir <- "../../../saved/survey"
source("src/runMods.R")
library(RColorBrewer)
library(forestFloor)
load(file=file.path(save.dir, "fs.Rdata"))
fs <- fs[fs$Round == 1,]

keep.RWA <- "wRWA"

if(keep.RWA == "woRWA"){
    fs <- fs[fs$Country != "RWA",]
}
## continuous
ys <- c("CSI",
        "hfias", "diversity")

xvars <- c("tenure",
           "Country",
           "total_paid_wages",
           "extension", "prop_pot_labor", "Landscape..",
           "total_crop_area",
           "org_fert",
           "inorg_fert",
           "pest_herb",
           "total_div",
           "livestock_int",
           "crop_rotation",
           "intercrop", "total_crop_rich",
           "crop_div",
           ## "simpson.div",
           "prop_education", "profit",
           "long_rainy_irrigation",
           "water_source_rainy",
           "water_source_dry",
           "water_insecurity",
           "long_rainy_crop_value_area",
           "short_rainy_crop_value_area",
           "long_rainy_yield_area", "short_rainy_yield_area",
           "pop_density")


formulas.tree <-lapply(ys, function(x) {
    as.formula(paste(x, "~", paste(paste(xvars, collapse="+"))))
})

fs.sub <- fs[!apply(fs[,ys], 1, function(x) any(is.na(x))),]

## fill in NAs using tree
filled.data <- lapply(formulas.tree,  rfImpute,
                      data=fs.sub,
                      ntree=1000,
                      niter=10)


## training and testing data
dev.val.data <- lapply(filled.data, sepDevValData)
fs.dev <- lapply(dev.val.data, function(x) x$dev)
fs.val <- lapply(dev.val.data, function(x) x$val)


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
    b=fs.dev,
    SIMPLIFY=FALSE)

## Predict Response Variable Value using Random Forest
f <- function(){
    plot(ff, col=Col, plot_GOF = TRUE, plot_seq = 1:15,
         orderByImportance=TRUE)
}

val.forests <- vector(mode="list", length=length(practice.forest))

for(i in 1:length(practice.forest)){
    val.forests[[i]] <- checkRandomForest(dev.data=fs.dev[[i]],
                                          val.data=fs.val[[i]],
                                          rf=practice.forest[[i]],
                                          plot.main=ys[i], y=ys[i])
    ff <- forestFloor(practice.forest[[i]],
                      fs.dev[[i]],
                      calc_np=TRUE)
    Col <- fcol(ff, cols=1, outlier.lim = 2.5)
    pdf.f(f, file.path(sprintf("figures/foodInsecurity/%s/%s.pdf",
                               keep.RWA, ys[i])),
          height=11)
}
