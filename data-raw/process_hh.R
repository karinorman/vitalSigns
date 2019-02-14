# rm(list=ls())
# setwd("~/Dropbox/vitalSigns/analysis/vital_signs")
# save.dir <- "../../saved/survey"

getData <- function(data.name, file.list, data.dir){
  dats <- paste(data.dir, file.list[grepl(data.name, file.list)],
                sep="/")
  dats.csv <- lapply(dats, read.csv)
  dats.csv <- do.call(rbind, dats.csv)
  return(dats.csv)
}

shared.names.hh <-  c("Country", "Partner.Organization", "longitude",
                      "latitude", "Landscape..", "Household.ID",
                      "Data.entry.date", "Individual.ID")


## *****************************************************************
## household data
## *****************************************************************

data.dir.hh <- system.file("extdata/survey/hh_data/raw", package = "vitalSigns")
hh.data <- list.files(data.dir.hh)

section.names.hh <- c("secA", "secB", "secC", "secE", "secHV1", "secHV2",
                      "sec_I", "secJ1", "secJ2", "secK1", "secK2",
                      "secL", "secN", "secU")


all.hh <- lapply(section.names.hh, getData, data.dir=data.dir.hh,
                 file.list=hh.data)

indiv.data <- sapply(all.hh, function(x) all(shared.names.hh %in% colnames(x)))
indiv.lev <- all.hh[indiv.data]
house.lev <- all.hh[!indiv.data]

indiv.merge <- Reduce(function(x, y) merge(x, y, by=shared.names.hh),
                      indiv.lev)

house.merge <- Reduce(function(x, y) merge(x, y,
                                           by=shared.names.hh[-8]),
                      house.lev)

hh <- merge(indiv.merge, house.merge)

hh <- hh[!duplicated(hh),]

usethis::use_data(hh)

## *****************************************************************
## ag. data
## *****************************************************************

data.dir.ag <- system.file("extdata/survey/ag_data", package = "vitalSigns")
ag.data <- list.files(data.dir.ag)


section.names.ag <- c("field_roster",
                      "sec3_field_details",
                      "sec4_crops_by_field", "crops_by_hh",
                      "permanent_crops_by_field",
                      "permanent_crops_by_crop", "byproducts",
                      "livestock_by_field", "10a_livestock",
                      "10b_livestock", "implements", "12_extension",
                      "extension_family", "secA_")

all.ag <- lapply(section.names.ag, getData, data.dir=data.dir.ag,
                 file.list=ag.data)

all.ag <- lapply(all.ag, function(x){
  x$Crop.type <- NULL
  return(x)
})


## indiv.lev <- sapply(all.ag, function(x) "Individual.ID" %in%
##                                         colnames(x))
## indiv.merge <- Reduce(function(x, y) merge(x, y),
##                       all.ag[indiv.lev])


field.lev <- sapply(all.ag, function(x) "Field.ID" %in% colnames(x))
field.merge <- Reduce(function(x, y) merge(x, y),
                      all.ag[field.lev])


crop.lev <- sapply(all.ag, function(x) "Crop.ID" %in%
                     colnames(x)) & !field.lev
crop.merge <- Reduce(function(x, y) merge(x, y),
                     all.ag[crop.lev])

house.lev <- !field.lev & !crop.lev
house.merge <- Reduce(function(x, y) merge(x, y),
                      all.ag[house.lev])


## data.prep1 <- merge(indiv.merge, field.merge, all=TRUE)
data.prep1 <- merge(field.merge, crop.merge, all=TRUE)
ag <- merge(data.prep1, house.merge, all=TRUE)

ag <- ag[!duplicated(ag),]

usethis::use_data(ag)