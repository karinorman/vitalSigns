# set working directory for analysis code
setwd("~/Dropbox/vitalSigns/analysis/vital_signs")

# prep to combine raw data files
data.dir <- "../../data/Agricultural - Household/hh_data/raw files"
save.dir <- "../../saved/survey"

hh.data <- list.files(data.dir)


getData <- function(data.name, file.list, data.dir){
    dats <- paste(data.dir, file.list[grepl(data.name, file.list)],
                  sep="/")
    dats.csv <- lapply(dats, read.csv)
    dats.csv <- do.call(rbind, dats.csv)
    return(dats.csv)
}

shared.colnames <-  c("Country", "Partner.Organization", "longitude",
                      "latitude", "Landscape..", "Household.ID",
                      "Data.entry.date", "Individual.ID")

section.names <- c("secA", "secB", "secC", "secE", "secHV1", "secHV2",
                   "sec_I", "secJ1", "secJ2", "secK1", "secK2",
                   "secL", "secN", "secU")


all.hh <- lapply(section.names, getData, data.dir=data.dir, file.list=hh.data)


indiv.data <- sapply(all.hh, function(x) all(shared.colnames %in% colnames(x)))
indiv.lev <- all.hh[indiv.data]
house.lev <- all.hh[!indiv.data]

indiv.merge <- Reduce(function(x, y) merge(x, y, by=shared.colnames),
                      indiv.lev)

house.merge <- Reduce(function(x, y) merge(x, y,
                                           by=shared.colnames[-8]),
                      house.lev)

hh <- merge(indiv.merge, house.merge)

save(hh, file=file.path(save.dir, "hh.Rdata"))
