library(randomForest)
library(lme4)
library(caret)

source(here("analysis", "src", "runMods.R"))

load(here("data", "ag.rda"))

ag <- ag[ag$Round == 1,]

ys <- c("ext_input", "no_inputs", "total_div", "any_div")

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
val.forests <- vector(mode="list", length=length(practice.forest))

for(i in 1:length(practice.forest)){
    val.forests[[i]] <- checkRandomForest(dev.data=ag.dev[[i]],
                                          val.data=ag.val[[i]],
                                          rf=practice.forest[[i]],
                                          plot.main=ys[i], y=ys[i])
}

## *****************************************************************
library(forestFloor)

for(i in 1:length(practice.forest)){
    X11(height=11)
    ff <- forestFloor(practice.forest[[i]],
                      ag.dev[[i]],
                      calc_np=TRUE)
    Col <- fcol(ff, cols=1, outlier.lim = 2.5)

    plot(ff, col=Col, plot_GOF = TRUE, plot_seq = 1:10,
         orderByImportance=TRUE)

}


