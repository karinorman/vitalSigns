rm(list=ls())
setwd("~/Dropbox/vitalSigns/analysis/vital_signs")
library(dplyr)
save.dir <- "../../saved/survey"


## *****************************************************************
## ag. data
## *****************************************************************

load(file.path(save.dir, "ag.Rdata"))

## simplify tenure classes
## set "shared-own" equal to "owned" (1)
ag$tenure <- ag$ag3a_14
ag$tenure[ag$tenure == 5] <- 1
## set non-ownership categories equal to 0
ag$tenure[ag$tenure > 1] <- 0


to.replace <- c("ag3a_07", "ag3a_08_1", "ag3a_18", "ag3a_18b",
                "ag3a_23", "ag3a_24", "ag3a_33","ag3a_34",
                "ag3a_25", "ag3a_19", "ag3a_35_1", "ag3a_35_2",
                "ag2a_vs_8b", "ag2a_vs_8c", "ag2a_04")

colnames(ag)[match(to.replace, colnames(ag))] <- c("erosion",
                                                "eros_type",
                                                "org_fert",
                                                "org_fert_type",
                                                "inorg_fert",
                                                "inorg_fert_type",
                                                "pest_herb",
                                                "pest_herb_type",
                                                "inorg_fert_amount",
                                                "org_fert_amount",
                                                "pest_herb_amount",
                                                "pest_herb_unit",
                                                "field_steps",
                                                "person_step",
                                                "farmer_field_est")

ag$org_fert_den <- ag$org_fert/ag$farmer_field_est
ag$inorg_fert_den <- ag$inorg_fert/ag$farmer_field_est

save(ag, file=file.path(save.dir, "ag.Rdata"))

## *****************************************************************
## household data
## *****************************************************************

load(file.path(save.dir, "hh.Rdata"))


"sex"=b$hh_b02,
"dob"=b$hh_b03
"hhhead"=b$hh_b05


## code "sand and bricks", "bricks and sand" as mud brick (4)
## code "Mud,poles,bricks and cement" as poles and mud (2)
## code "Baked bricks and and sand" as baked bricks (5)

## code "bush" as "no toilet" (1)
