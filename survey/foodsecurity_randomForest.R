rm(list=ls())
library(randomForest)
setwd("~/Dropbox/vitalSigns/analysis/vital_signs/survey")
save.dir <- "../../../saved/survey"
source("src/runMods.R")
library(RColorBrewer)

load(file=file.path(save.dir, "ag.Rdata"))

## ag <- ag[ag$Country != "RWA",]

## continuous
ys <- c("food_insecurity")

xvars <- c("tenure",
           "Country",
           "total_paid_wages",
           "extension", "prop_pot_labor", "Landscape..",
           "total_crop_area",
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
           "water_insecurity",
           ## "electricity_source",
           ## "lighting_fuel",
           ## "cooking_fuel",
           "long_rainy_soil_quality")


insec.form <-  as.formula(paste(ys, "~",
                                paste(paste(xvars, collapse="+"))))

## drop varaibles that have too many NAs?
ag.sub.insec <- ag[!is.na(ag[,ys]),]

## apply(ag.sub.insec[, xvars], 2, function(x) length(x[!is.na(x)]))
apply(ag.sub.insec[, xvars], 2, table)

ag.sub.insec <- rfImpute(x=insec.form,
                         data=ag.sub.insec,
                         ntree=1000,
                         niter=10)

## training and testing data
set.seed(20)

sample.ind <- sample(2,
                     nrow(ag.sub.insec),
                     replace = TRUE,
                     prob = c(0.8,0.2))

ag.dev.insec <- ag.sub.insec[sample.ind==1,]
ag.val.insec <- ag.sub.insec[sample.ind==2,]

food.insec.forest <- randomForest(insec.form,
                                  data=ag.dev.insec,
                                  importance=TRUE,
                                  sampsize=25,
                                  ntree=5000,
                                  mtry=4,
                                  keep.inbag = TRUE,
                                  keep.forest = TRUE,
                                  proximity=TRUE
                                  ## na.action=na.roughfix
                                  )


food.insec <- checkRandomForest(dev.data=ag.dev.insec,
                                val.data=ag.val.insec,
                                rf=food.insec.forest,
                                plot.main="Food insecurity",
                                y=ys)

## *****************************************************************
library(forestFloor)
ff <- forestFloor(food.insec.forest,
                  ag.dev.insec,
                  calc_np=TRUE)
Col <- fcol(ff, cols=1, outlier.lim = 2.5)

f <- function(){
    plot(ff, col=Col, plot_GOF = TRUE, plot_seq = 1:15,
         orderByImportance=TRUE)
}

pdf.f(f, file.path("figures/woRWA", sprintf("%s.pdf", "ff")),
      height=11)

## show3d(ff, col=Col, plot_GOF=FALSE)
## *****************************************************************


plotRF <- function(this.pred, data, predictor, response,
                   ylims=c(0,1), pretty.breaks, cols, first.levs, first.pred, ...){
    these.data <- data[data[, predictor] == this.pred,]
    pretty.breaks <- pretty(range(data[, response], na.rm=TRUE), min.n=2)
    plot(NULL, ylim=ylims, xlim=range(pretty.breaks),
         xlab="", ylab="", bty="n", ...)
    ## legend("topleft", legend= first.levs, col=cols, bty="n", pch=15)
    for(lev in first.levs){
        ## d.y <- try(density(these.data[, response][these.data[, first.pred] == lev],
        ##                    na.rm=TRUE), silent=TRUE)
        ## if(inherits(d.y, "try-error")) next
        ## ## points(y=d.y$y, x=d.y$x, type='l', ltd=5,
        ## ##        col=cols[lev])
        ## polygon(d.y,
        ##         col=cols[lev])

        d.y <- try(hist(these.data[, response][these.data[, first.pred] == lev],
                        add=TRUE, freq=FALSE, breaks=pretty.breaks, col=cols[lev]), silent=TRUE)

        ## d.y <- try(hist(these.data[, response][these.data[, first.pred] == lev],
        ##                 plot=FALSE, frequency=FALSE, breaks=pretty.breaks), silent=TRUE)
        if(inherits(d.y, "try-error")) next
        ## barplot(rev(d.y$density),rev(d.y$breaks),horiz=TRUE, space=0,
        ##         xlim=xlims, col=cols[lev], add=TRUE)
    }
    text((max(pretty.breaks)/2), par("usr")[3] - 0.1, srt = 45, adj = 1,
         labels = this.pred, xpd = TRUE, cex=1.5)


}

important.varibles <- names(sort(ff$importance, decreasing=TRUE)[2:15])


plotVars <- function(){
    layout(matrix(1:length(levels(ag[,k])), nrow=1))
    par(oma=c(1,2,1,1),
        mar=c(10,2,0,0))
    first.levs <- levels(ag[, "Country"])
    cols <- rainbow(length(first.levs), alpha=0.5)
    names(cols) <- first.levs
    for(lev in levels(ag[,k])){
        plotRF(lev,
               ag,  predictor=k,
               response="food_insecurity",
               ## yaxt="n",
               ylims=c(0,1),
               cols=cols,
               first.levs=first.levs,
               first.pred="Country",
               pretty.breaks = pretty(range(ag$food_insecurity,
                                            na.rm=TRUE)))
        legend("topleft", legend= first.levs, col=cols, bty="n", pch=15)

    }
}

for(k in important.varibles){
    pdf.f(plotVars, file.path("figures/woRWA", sprintf("%s.pdf", k)),
          width=30)
}



boxplot(ag.dev.insec$food_insecurity ~ ag.dev.insec$Country)
## boxplot(ag.dev.insec$food_insecurity ~ ag.dev.insec$Landscape..)

## boxplot(ag.dev.insec$food_insecurity ~ ag.dev.insec$water_source_dry)
## boxplot(ag.dev.insec$food_insecurity ~ ag.dev.insec$water_source_rainy)

## boxplot(ag.dev.insec$food_insecurity ~ ag.dev.insec$long_rainy_inorg_fert_type)
## boxplot(ag.dev.insec$food_insecurity ~ ag.dev.insec$short_rainy_inorg_fert_type)

## boxplot(ag.dev.insec$food_insecurity ~
## ag.dev.insec$short_rainy_pest_herb_type)


## boxplot(ag.dev.insec$food_insecurity ~
##             ag.dev.insec$long_rainy_org_fert_type)

## boxplot(ag.dev.insec$food_insecurity ~
##             ag.dev.insec$long_rainy_irrigation)


## boxplot(ag.dev.insec$food_insecurity ~ ag.dev.insec$long_rainy_pest_herb_type)
## boxplot(ag.dev.insec$food_insecurity ~ ag.dev.insec$org_fert)

## plot(ag.dev.insec$food_insecurity ~ ag.dev.insec$total_div)
## boxplot(ag.dev.insec$food_insecurity ~ ag.dev.insec$short_rainy_org_fert_type)


## rowanda has higher food insecutiry over all
## subsidized water vending station, unprotected well with pump and
## protected well without pump all have high insecurity
## farmers that use DAP (look like they are mostly from Rowanda)
