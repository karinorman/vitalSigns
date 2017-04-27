rm(list=ls())
setwd("~/Dropbox/vitalSigns/analysis/vital_signs")
library(dplyr)
save.dir <-

## *****************************************************************
## ag. data
## *****************************************************************

ag <- read.csv("~/Dropbox/vitalSigns/data/joinedData/ag_field.csv")
extension <- read.csv("~/Dropbox/vitalSigns/data/joinedData/ag_extension.csv")

to.replace <-  c("ag3a_14", ## tenure
                 "ag3a_06", ## soil quality
                 "ag3a_07", "ag3a_08_1", ## erosion
                 "ag3a_18","ag3a_18b",  "ag3a_19", ## org fert
                 "ag3a_23", "ag3a_24",  "ag3a_25", ## in org fert
                 "ag3a_33", "ag3a_34", "ag3a_35_1",  "ag3a_35_2",  ## pest/herb
                 "ag3a_26", ## fertilizer certificate
                 "ag3a_38_3", "ag3a_38_6", "ag3a_38_64", "ag3a_38_9") ## wages

to.replace <- c(paste("long_rainy_", to.replace, sep=""),
                paste("short_rainy_", to.replace, sep=""),
                "ag10_vs_1", "ag2a_09")

practices <- c("tenure",
               "soil_quality",
               "erosion","eros_type",
               "org_fert", "org_fert_type", "org_fert_amount",
               "inorg_fert", "inorg_fert_type", "inorg_fert_amount",
               "pest_herb", "pest_herb_type", "pest_herb_amount", "pest_herb_unit",
               "fertCertificate", "wages_landprep", "wages_weeding",
               "wages_non-harvest", "wages_harvesting")

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

## *****************************************************************
## sum diversification practices
ag$totalDiv <- sum(ag$div_treebelt, ag$org_fert, na.rm=TRUE)

## *****************************************************************
## simplify tenure classes
## set "shared-own" equal to "owned" (1)
ag$tenure[ag$tenure == 5] <- TRUE
## set non-ownership categories equal to 0
ag$tenure[ag$tenure > 1] <- FALSE

## *****************************************************************
## extension

ag$extension <- extension$source_id[match(ag$Household.ID,
                                          extension$Household.ID)]

ag$extension_source <- extension$source_name[match(ag$Household.ID,
                                          extension$Household.ID)]

save(ag, file=file.path(save.dir, "ag.Rdata"))

## *****************************************************************
## household data
## *****************************************************************

## load(file.path(save.dir, "hh.Rdata"))




## "sex"=b$hh_b02,
## "dob"=b$hh_b03
## "hhhead"=b$hh_b05


## code "sand and bricks", "bricks and sand" as mud brick (4)
## code "Mud,poles,bricks and cement" as poles and mud (2)
## code "Baked bricks and and sand" as baked bricks (5)

## code "bush" as "no toilet" (1)
