library(randomForest)
library(lme4)
library(rpart)
library(ranger)

source(here("analysis", "src", "runMods.R"))

load(here("data", "ag.rda"))

ys <- c("any_div", "ext_input", "no_inputs", "total_div")
fams <- c(rep("binomial", length(ys) -1), "poisson")

## also extension can be a factor with 8 levels ("no extension not an
## option?")

xvar <- c("as.factor(tenure)",
          "Country",
          "scale(total_paid_wages)",
          "any_extension", "scale(hh_pot_labor)", "scale(total_crop_area)")

random.variables <- c("Household.ID", "Landscape..")

all.vars <- c(xvar, random.variables)

formulas <-lapply(ys, function(x) {
    as.formula(paste(x, "~", paste(paste(xvar, collapse="+"),
                                   paste0("(1|", random.variables,
                                          ")"), sep="+")))
})


out.mods <- mapply(function(a, b, c)
    runMod(forms= a,
           fam= b,
           ys=c,
           dats=ag),
    a=formulas,
    b=fams,
    c=ys,
    SIMPLIFY=FALSE)

names(out.mods) <- ys
lapply(out.mods, summary)

## *********************************************************
## recursive partitioning
## *********************************************************

xvars <- c("tenure",
           "Country",
           "total_paid_wages",
           "extension", "hh_pot_labor", "Landscape..", "total_crop_area")


methods <- c(rep("class", length(ys) -1), "poisson")

formulas.tree <-lapply(ys, function(x) {
    as.formula(paste(x, "~", paste(paste(xvars, collapse="+"))))
})


## ag.sub <- ag[apply(ag[, xvars], 1, function(x) all(!is.na(x))),]

out.mods.trees <- mapply(function(a, b)
    rpart(formula= a,
           method= b,
           data=ag),
    a=formulas.tree,
    b=methods,
    SIMPLIFY=FALSE)


lapply(out.mods.trees, summary, cp=TRUE)

layout(matrix(1:(2*round(length(out.mods.trees)/2)), nrow=2))
lapply(out.mods.trees, function(x){
    try(plot(x), silent=TRUE)
    try(text(x, use.n=TRUE), silent=TRUE)
    })

## dr <- randomForest(y=ag[, ys],
##              x=ag[, xvars],
##              importance=TRUE,
##              na.action=na.omit)

