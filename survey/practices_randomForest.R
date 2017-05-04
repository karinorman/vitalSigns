rm(list=ls())
library(randomForest)
library(lme4)
library(rpart)
setwd("~/Dropbox/vitalSigns/analysis/vital_signs/survey")
save.dir <- "../../../saved/survey"
source("src/runMods.R")

load(file=file.path(save.dir, "ag.Rdata"))

ys <- c("ext_input", "no_inputs")

xvars <- c("tenure",
           "Country",
           "total_paid_wages",
           "extension",
           "prop_pot_labor",
           "Landscape..",
           "total_crop_area",
           "prop_education",
           "profit")



formulas.tree <-lapply(ys, function(x) {
    as.formula(paste(x, "~", paste(paste(xvars, collapse="+"))))
})

ag.sub <- ag[!apply(ag[,ys], 1, function(x) any(is.na(x))),]

## training and testing data
sample.ind <- sample(2,
                     nrow(ag.sub),
                     replace = TRUE,
                     prob = c(0.6, 0.4))

ag.dev <- ag.sub[sample.ind==1,]
ag.val <- ag.sub[sample.ind==2,]


practice.forest <- lapply(formulas.tree,
                          randomForest,
                          data=ag.dev,
                          importance=TRUE, ntree=500,
                          na.action=na.omit)

## Predict Response Variable Value using Random Forest

val.forests <- vector(mode="list", length=length(practice.forest))

for(i in 1:length(practice.forest)){
    val.forests[[i]] <- checkRandomForest(dev.data=ag.dev,
                                          val.data=ag.val,
                                          rf=practice.forest[[i]],
                                          plot.main=ys[i], y=ys[i])
}

