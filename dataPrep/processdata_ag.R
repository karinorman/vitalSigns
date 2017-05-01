rm(list=ls())
setwd("~/Dropbox/vitalSigns/analysis/vital_signs")
library(dplyr)
library(lubridate)
save.dir <- "../../saved/survey"

## *****************************************************************
## ag. data
## *****************************************************************

ag <- read.csv("~/Dropbox/vitalSigns/data/joinedData/ag_field.csv")
extension <-
    read.csv("~/Dropbox/vitalSigns/data/joinedData/ag_extension.csv")
ind <- read.csv("~/Dropbox/vitalSigns/data/Agricultural - Household/hh_data/hh_secB.csv")
hh <-
    read.csv("~/Dropbox/vitalSigns/data/joinedData/hh.csv")


to.replace <-  c("ag3a_14", ## tenure
                 "ag3a_06", ## soil quality
                 "ag3a_07", "ag3a_08_1", ## erosion
                 "ag3a_18","ag3a_18b",  "ag3a_19", ## org fert
                 "ag3a_23", "ag3a_24",  "ag3a_25", ## in org fert
                 "ag3a_33", "ag3a_34", "ag3a_35_1",  "ag3a_35_2",  ## pest/herb
                 "ag3a_26", ## fertilizer certificate
                 "ag3a_38_3", "ag3a_38_6", "ag3a_38_64", "ag3a_38_9", ##wages
                 "ag4a_04", ## intercropping
                 "ag3a_09", "ag3a_10") ##irrigation

to.replace <- c(paste("long_rainy_", to.replace, sep=""),
                paste("short_rainy_", to.replace, sep=""),
                "ag10_vs_1", "ag2a_09")

practices <- c("tenure",
               "soil_quality",
               "erosion","eros_type",
               "org_fert", "org_fert_type", "org_fert_amount",
               "inorg_fert", "inorg_fert_type", "inorg_fert_amount",
               "pest_herb", "pest_herb_type", "pest_herb_amount", "pest_herb_unit",
               "fert_cert", "wages_landprep", "wages_weeding",
               "wages_non-harvest", "wages_harvesting",
               "intercrop",
               "irrigation", "irrigation_type")

colnames(ag)[match(to.replace, colnames(ag))] <-
    c(paste("long_rainy_",
            practices, sep=""),
      paste("short_rainy_", practices, sep=""),
      "livestock_int",
      "GPS_area")

## *****************************************************************
## if the farm uses tree belts for erosion control in either season
ag$div_treebelt <- NA
ag$div_treebelt <- ag$short_rainy_eros_type == "5" |
    ag$long_rainy_eros_type == "5"

ag$div_treebelt <- NA
ag$div_treebelt <- ag$short_rainy_eros_type == "5" |
    ag$long_rainy_eros_type == "5"


## *****************************************************************
## if the farm intercrops at least one season
ag$intercrop <- NA
ag$intercrop <- ag$short_rainy_intercrop == 1 &
    !is.na(ag$short_rainy_intercrop) |
    ag$long_rainy_intercrop == 1 &
    !is.na(ag$long_rainy_intercrop)

## *****************************************************************
## if the farm uses inorganic fertilizer
ag$inorg_fert <- NA
ag$inorg_fert <- ag$short_rainy_inorg_fert == 1 &
    !is.na(ag$short_rainy_inorg_fert) |
    ag$long_rainy_inorg_fert == 1 &
    !is.na(ag$long_rainy_inorg_fert)

## *****************************************************************
## if the farm uses organic fertilizer
ag$org_fert <- NA
ag$org_fert <- ag$short_rainy_org_fert == 1 &
    !is.na(ag$short_rainy_org_fert) |
    ag$long_rainy_org_fert == 1 &
    !is.na(ag$long_rainy_org_fert)

## *****************************************************************
## if the farm uses pesticides or herbicides
ag$pest_herb <- NA
ag$pest_herb <- ag$short_rainy_pest_herb == 1 &
    !is.na(ag$short_rainy_pest_herb) |
    ag$long_rainy_pest_herb == 1 &
    !is.na(ag$long_rainy_pest_herb)

## *****************************************************************
## certification for external inputs
ag$fert_cert <- NA
ag$fert_cert <- ag$short_rainy_fert_cert == 1 &
    !is.na(ag$short_rainy_fert_cert) |
    ag$long_rainy_fert_cert == 1 &
    !is.na(ag$long_rainy_fert_cert)

## *****************************************************************
## any external inputs?
ag$ext_input <- NA
ag$ext_input <- ag$pest_herb == TRUE  |
    ag$inorg_fert == TRUE

## *****************************************************************
## livestock integration
ag$livestock_int[ag$livestock_int == 1] <- TRUE
ag$livestock_int[ag$livestock_int == 1] <- FALSE

## *****************************************************************
## wages
wage.cols <- c(paste("long_rainy_",
                     c("wages_landprep", "wages_weeding",
                       "wages_non-harvest",
                       "wages_harvesting"),
                     sep=""),
               paste("short_rainy_",
                     c("wages_landprep", "wages_weeding",
                       "wages_non-harvest",
                       "wages_harvesting"),
                     sep=""))
## total wages paid
ag$total_paid_wages <- apply(ag[, wage.cols], 1, sum, na.rm=TRUE)

ag$total_paid_wages[apply(ag[, wage.cols], 1, function(x)
    all(is.na(x)))] <- NA

## *****************************************************************
## add NAs back ot the right places
ag$pest_herb[is.na(ag$short_rainy_pest_herb) &
             is.na(ag$long_rainy_pest_herb)] <- NA
ag$inorg_fert[is.na(ag$short_rainy_inorg_fert) &
              is.na(ag$long_rainy_inorg_fert)] <- NA
ag$ext_input[is.na(ag$inorg_fert) &
             is.na(ag$pest_herb)] <- NA
ag$org_fert[is.na(ag$short_rainy_org_fert) &
            is.na(ag$long_rainy_org_fert)] <- NA
ag$fert_cert[is.na(ag$short_rainy_fert_cert) &
             is.na(ag$long_rainy_fert_cert)] <- NA
ag$intercrop[is.na(ag$short_rainy_intercrop) &
             is.na(ag$long_rainy_intercrop)] <- NA


## *****************************************************************
## simplify tenure classes
## set "shared-own" equal to "owned" (1)
ag$tenure <- ag$long_rainy_tenure
ag$tenure[ag$tenure == 5] <- TRUE
## set non-ownership categories equal to 0
ag$tenure[ag$tenure > 1] <- FALSE
ag$tenure[is.na(ag$long_rainy_tenure)] <- NA

## *****************************************************************
## extension

ag$extension <- as.character(extension$source_id[match(ag$Household.ID,
                                          extension$Household.ID)])
ag$extension[is.na(ag$extension)] <- 0

ag$any_extension <- ag$extension
ag$any_extension[ag$any_extension != 0] <- 1

ag$extension_source <- extension$source_name[match(ag$Household.ID,
                                                   extension$Household.ID)]

## *****************************************************************
## crop rotations
ag$crop_rotation <- as.numeric(ag$long_rainy_Crop.ID) !=
    as.numeric(ag$short_rainy_Crop.ID)

## *****************************************************************
## crop richness
ag$long_rainy_crop_rich <- tapply(ag$long_rainy_Crop.ID,
                                  ag$Household.ID,
                                  function(x) length(unique(x)))

ag$short_rainy_crop_rich <- tapply(ag$short_rainy_Crop.ID,
                                   ag$Household.ID,
                                   function(x) length(unique(x)))

ag$total_crop_rich <- apply(ag[, c("long_rainy_crop_rich",
                                  "short_rainy_crop_rich")], 1, sum,
                           na.rm=TRUE)

ag$total_crop_area <- tapply(ag$GPS_area, ag$Household.ID, sum)

ag$crop_div <- tapply(ag$GPS_area, ag$Household.ID,
                      function(x) -sum(x*log(x)))

## *****************************************************************
## sum diversification practices
div.cols <- c("div_treebelt", "org_fert", "livestock_int",
              "crop_rotation", "intercrop")

ag$total_div <- apply(ag[, div.cols], 1, sum, na.rm=TRUE)
ag$total_div[apply(ag[, div.cols], 1,
                   function(x) all(is.na(x)))] <- NA

ag$any_div <- ag$total_div > 0

## *****************************************************************
## farmers that don't use diversification practices or external inputs

ag$no_inputs <- ag$any_div | ag$ext_input
ag$no_inputs[apply(ag[, c("any_div", "ext_input")], 1,
                   function(x) all(is.na(x)))] <- NA



## *****************************************************************
## get classes correct
made.cols.factor <- c("any_div", "ext_input", "no_inputs",
                      "crop_rotation", "intercrop",
                      "fert_cert", "pest_herb", "inorg_fert", "org_fert")

ag[, made.cols.factor] <- ag[, made.cols.factor]*1



## *****************************************************************
## household data
## *****************************************************************

## *****************************************************************
## number of indivudals over 5 in a household and under 75?

## sex hh_b02
## dob hh_b03
## "hhhead"=b$hh_b05

ind$dob <- as.Date(ind$hh_b03)
ind$data_date <- as.Date(ind$Data.entry.date)
ind$yrs_old <- year(ind$data_date) - year(ind$dob)
ind$working <- ind$yrs_old > 4 & ind$yrs_old < 75

hh.labor <- tapply(ind$working, ind$Household.ID, sum, na.rm=TRUE)

## potential farm help?
ag$hh_pot_labor <- hh.labor[match(ag$Household.ID, names(hh.labor))]

## *****************************************************************
## food insecurity

## hh_refno == Household.ID
food.insecurity <- c("hh_i01", "hh_i02_1", "hh_i02_2", "hh_i02_3",
                     "hh_i02_4", "hh_i02_5", "hh_i02_6", "hh_i02_7",
                     "hh_i02_8")

ag <- cbind(ag, hh[, food.insecurity][match(ag$Household.ID, hh$hh_refno),])

## code "sand and bricks", "bricks and sand" as mud brick (4)
## code "Mud,poles,bricks and cement" as poles and mud (2)
## code "Baked bricks and and sand" as baked bricks (5)

## code "bush" as "no toilet" (1)


save(ag, file=file.path(save.dir, "ag.Rdata"))
