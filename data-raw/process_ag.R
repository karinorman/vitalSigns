library(dplyr)
library(lucr)
library(lubridate)

## *****************************************************************
## ag. data
## *****************************************************************

ag <- read.csv("~/Dropbox/vitalSigns/data/joinedData/ag_field.csv")

extension <- read.csv("~/Dropbox/vitalSigns/data/joinedData/ag_extension.csv")

ind <- read.csv(system.file("extdata/survey/hh_data", "hh_secB.csv", package = "vitalSigns"))

hh <- read.csv("~/Dropbox/vitalSigns/data/joinedData/hh.csv")

yield <- read.csv(system.file("extdata", "Yields_Combine_export.csv", package = "vitalSigns"))

ag$Landscape.. <- paste0(ag$Country, ag$Landscape..)

to.replace <-  c("ag3a_14", ## tenure
                 "ag3a_06", ## soil quality
                 "ag3a_07", "ag3a_08_1", ## erosion
                 "ag3a_18","ag3a_18b",  "ag3a_19", ## org fert
                 "ag3a_23", "ag3a_24",  "ag3a_25", ## in org fert
                 "ag3a_33", "ag3a_34", "ag3a_35_1",  "ag3a_35_2",  ## pest/herb
                 "ag3a_26", ## fertilizer certificate
                 "ag3a_38_3", "ag3a_38_6", "ag3a_38_64", "ag3a_38_9", ##wages
                 "ag4a_04", ## intercropping
                 "ag3a_09", "ag3a_10", "ag3a_11", ##irrigation/water
                 "ag4a_15",  "ag4a_15_unit","ag4a_16") #crop yield

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
               "irrigation", "irrigation_type", "field_water_source",
               "yield", "yield_unit", "crop_value")

colnames(ag)[match(to.replace, colnames(ag))] <-
    c(paste("long_rainy_",
            practices, sep=""),
      paste("short_rainy_", practices, sep=""),
      "livestock_int",
      "GPS_area")

## *****************************************************************
## yield
ag$long_rainy_yield_area <- ag$long_rainy_yield/ag$GPS_area
ag$short_rainy_yield_area <- ag$short_rainy_yield/ag$GPS_area

ag$Date <- date(ag$Date)

# cur.year <- historic_currency(dates=unique(ag$Date[!is.na(ag$Date)]),
#                               key="7c415c728109467ab0b6861016e03780")
# 
# save(cur.year, file= 'data/inst/cur_year.Rdata')

load('data/int/cur_year.Rdata')

names(cur.year) <- unique(ag$Date[!is.na(ag$Date)])

currencies.to.get <- c("TZS", "UGX", "RWF", "GHS")

unique.cur <- t(sapply(cur.year, function(x){
    unlist(x$rates[names(x$rates) %in% currencies.to.get])
}))

ag$currency <- "GHS"
ag$currency[ag$Country == "TZA"] <- "TZS"
ag$currency[ag$Country == "RWA"] <- "RWF"
ag$currency[ag$Country == "UGA"] <- "UGX"

ag <- cbind(ag, unique.cur[match(ag$Date,
                                 date(rownames(unique.cur))),])

ag$to_convert <- apply(ag, 1, function(x) x[x["currency"]])

ag$long_rainy_crop_value_area<- ag$long_rainy_crop_value/as.numeric(ag$to_convert)/
    ag$GPS_area
ag$short_rainy_crop_value_area <- ag$short_rainy_crop_value/as.numeric(ag$to_convert)/ag$GPS_area

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
## if the farm uses erosion control at least one season
ag$erosion <- NA
ag$erosion <- ag$short_rainy_erosion == 1 &
    !is.na(ag$short_rainy_erosion) |
    ag$long_rainy_erosion == 1 &
    !is.na(ag$long_rainy_erosion)


ag$long_rainy_eros_type <-
    as.character(ag$long_rainy_eros_type)
ag$short_rainy_eros_type <-
    as.character(ag$short_rainy_eros_type)

ag$long_rainy_eros_type[ag$long_rainy_erosion == 2] <- 0
ag$short_rainy_eros_type[ag$short_rainy_erosion == 2] <- 0

## *****************************************************************
## if the farm uses inorganic fertilizer
ag$inorg_fert <- NA
ag$inorg_fert <- ag$short_rainy_inorg_fert == 1 &
    !is.na(ag$short_rainy_inorg_fert) |
    ag$long_rainy_inorg_fert == 1 &
    !is.na(ag$long_rainy_inorg_fert)

ag$long_rainy_inorg_fert_type <-
    as.character(ag$long_rainy_inorg_fert_type)
ag$short_rainy_inorg_fert_type <-
    as.character(ag$short_rainy_inorg_fert_type)

ag$long_rainy_inorg_fert_type[ag$long_rainy_inorg_fert == 2] <- 0
ag$short_rainy_inorg_fert_type[ag$short_rainy_inorg_fert == 2] <- 0



## *****************************************************************
## if the farm uses organic fertilizer
ag$org_fert <- NA
ag$org_fert <- ag$short_rainy_org_fert == 1 &
    !is.na(ag$short_rainy_org_fert) |
    ag$long_rainy_org_fert == 1 &
    !is.na(ag$long_rainy_org_fert)


ag$long_rainy_org_fert_type <-
    as.character(ag$long_rainy_org_fert_type)
ag$short_rainy_org_fert_type <-
    as.character(ag$short_rainy_org_fert_type)

ag$long_rainy_org_fert_type[ag$long_rainy_org_fert == 2] <- 0
ag$short_rainy_org_fert_type[ag$short_rainy_org_fert == 2] <- 0

## *****************************************************************
## if the farm uses pesticides or herbicides
ag$pest_herb <- NA
ag$pest_herb <- ag$short_rainy_pest_herb == 1 &
    !is.na(ag$short_rainy_pest_herb) |
    ag$long_rainy_pest_herb == 1 &
    !is.na(ag$long_rainy_pest_herb)


ag$long_rainy_pest_herb_type <-
    as.character(ag$long_rainy_pest_herb_type)
ag$short_rainy_pest_herb_type <-
    as.character(ag$short_rainy_pest_herb_type)

ag$long_rainy_pest_herb_type[ag$long_rainy_pest_herb == 2] <- 0
ag$short_rainy_pest_herb_type[ag$short_rainy_pest_herb == 2] <- 0

## *****************************************************************
## irrigation
ag$long_rainy_irrigation[ag$long_rainy_irrigation == 2] <- 0
ag$short_rainy_irrigation[ag$short_rainy_irrigation == 2] <- 0

ag$long_rainy_irrigation_type <-
    as.character(ag$long_rainy_irrigation_type)
ag$short_rainy_irrigation_type <-
    as.character(ag$short_rainy_irrigation_type)

ag$long_rainy_irrigation_type[ag$long_rainy_irrigation == 0] <- 0
ag$short_rainy_irrigation_type[ag$short_rainy_irrigation == 0] <- 0

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
ag$livestock_int[ag$livestock_int == 2] <- FALSE

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
ag$erosion[is.na(ag$short_rainy_erosion) &
             is.na(ag$long_rainy_erosion)] <- NA


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
ag$any_extension <- as.numeric(ag$any_extension)

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

div.cols <- c(## "div_treebelt",
              "org_fert", "livestock_int",
              ## "crop_rotation",
              "intercrop")

ag$total_div <- apply(ag[, div.cols], 1, sum, na.rm=TRUE)
ag$total_div[apply(ag[, div.cols], 1,
                   function(x) all(is.na(x)))] <- NA

ag$any_div <- ag$total_div > 0

## *****************************************************************
## farmers that don't use diversification practices or external inputs

ag$no_inputs <- !ag$org_fert & !ag$ext_input
ag$no_inputs[apply(ag[, c("any_div", "ext_input")], 1,
                   function(x) all(is.na(x)))] <- NA


## *****************************************************************
## household data
## *****************************************************************

## *****************************************************************
## number of individuals over 5 in a household and under 75?

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

## total individuals
total.ind <- tapply(ind$Individual.ID, ind$Household.ID, length)
ag$hh_ind <- total.ind[match(ag$Household.ID, names(total.ind))]

hh$hh_ind <- total.ind[match(hh$hh_refno, names(total.ind))]

## prop working?
ag$prop_pot_labor <- ag$hh_pot_labor/ag$hh_ind

## *****************************************************************
## household data
## *****************************************************************
## food insecurity

## hh_refno == Household.ID
food.insecurity <- c("hh_i01", "hh_i02_1", "hh_i02_2", "hh_i02_3",
                     "hh_i02_4", "hh_i02_5", "hh_i02_6", "hh_i02_7",
                     "hh_i02_8")

water.insecurity <- c("hh_j14", "hh_j15", ## water source
                      "j20b_02", ## water insecurity
                      "hh_j13", "hh_j12", "hh_j11") ## fuel/electricity
hh$j20b_02[hh$j20b_02 == 2] <- 0
hh$hh_c03[hh$j20b_02 == 0] <- 0
hh$hh_i01[hh$j20b_02 == 0] <- 0

hh$food_insecurity <- apply(hh[, food.insecurity], 1, sum)

insec.names <- c("water_source_rainy",
      "water_source_dry",
      "water_insecurity",
      "electricity_source",
      "lighting_fuel",
      "cooking_fuel")

colnames(hh)[match(water.insecurity, colnames(hh))] <- insec.names

ag <- cbind(ag,
            hh[, c(insec.names, "food_insecurity")][match(ag$Household.ID,
                                           hh$hh_refno),])


## educated inviduals
sec3 <- read.csv(system.file("extdata/survey/hh_data", "hh_secC.csv", package = "vitalSigns"))
sec3$hh_c03[sec3$hh_c03 == 2] <- 0

hh.edu <- tapply(sec3$hh_c03, sec3$Household.ID,
                 sum, na.rm=TRUE)

ag$educated <- hh.edu[match(ag$Household.ID, names(hh.edu))]

ag$prop_education <- ag$educated/ag$hh_ind

secE <- read.csv(system.file("extdata/survey/hh_data", "hh_secE.csv", package = "vitalSigns"))
hh.profit <- tapply(secE$hh_e65_1, sec3$Household.ID,
                    sum, na.rm=TRUE)

ag$profit <- hh.profit[match(ag$Household.ID, names(hh.profit))]

## *****************************************************************
## population density data
## *****************************************************************
load('data/int/gha_popstats.rdata')
load('data/int/rwa_popstats.rdata')
load('data/int/tanz_popstats.rdata')
load('data/int/ug_popstats.rdata')

gha.pop <- t(as.data.frame(popstats.mult.gha, stringsAsFactors = FALSE))
gha.pop <- as.data.frame(gha.pop, stringsAsFactors = FALSE)
gha.pop$Country <- "GHA"

rwa.pop <- t(as.data.frame(popstats.mult.rwa, stringsAsFactors = FALSE))
rwa.pop <- as.data.frame(rwa.pop, stringsAsFactors = FALSE)
rwa.pop$Country <- "RWA"

tza.pop <- t(as.data.frame(popstats.mult.tanz, stringsAsFactors = FALSE))
tza.pop <- as.data.frame(tza.pop, stringsAsFactors = FALSE)
tza.pop$Country <- "TZA"

uga.pop <- t(as.data.frame(popstats.mult.ug, stringsAsFactors = FALSE))
uga.pop <- as.data.frame(uga.pop, stringsAsFactors = FALSE)
uga.pop$Country <- "UGA"

all.pop.stats <- rbind(gha.pop, rwa.pop, tza.pop, uga.pop, stringsAsFactors = FALSE)
names(all.pop.stats)[names(all.pop.stats) == 'V1'] <- 'pop_density'

all.pop.stats$Landscape <- lapply(strsplit(rownames(all.pop.stats), split="\\."), function(x) x[1])

ag <- cbind(ag, all.pop.stats[ , -c(2)][match(ag$Landscape..,
                                             paste0(all.pop.stats$Country,
                                                    all.pop.stats$Landscape)), ])

## *****************************************************************
## spatial data
## *****************************************************************

load('data/int/tanz_stats.rdata')
load('data/int/gha_stats.rdata')
load('data/int/ug_stats.rdata')

getSpatialDiv <- function(spStats,
                          d,
                          keep.stat= c("simpson.div"),
                          by.site,
                          site.col= "Landscape.."){
    extract.stats <- function(ds.element, spStats){
        a.stats <- lapply(spStats, function(x) x[ds.element])
        mat.stats <- unlist(a.stats, recursive=FALSE)
        df.stats <- do.call(rbind, mat.stats)
        df.stats <- as.data.frame(df.stats)
        return(df.stats)
    }
    out.lms <- vector("list", length(d))
    for(i in 1:length(d)){
        out.stat <- extract.stats(ds.element=i,
                                  spStats=spStats)
        out.stat$Site <- gsub(paste0(".", d), "", rownames(out.stat))
    }
    return(out.stat)
}

all.sp.stats <- lapply(list(sum.tanz, sum.gha, sum.ug), getSpatialDiv,
                       d="1000",  by.site=ag)

all.sp.stats[[1]]$Country <- "TZA"
all.sp.stats[[2]]$Country <- "GHA"
all.sp.stats[[3]]$Country <- "UGA"

all.sp.stats <- do.call(rbind, all.sp.stats)
ag <- cbind(ag,
            all.sp.stats[ , -c(40:41)][match(ag$Landscape..,
                                             paste0(all.sp.stats$Country,
                                                   all.sp.stats$Site)),])



## code "sand and bricks", "bricks and sand" as mud brick (4)
## code "Mud,poles,bricks and cement" as poles and mud (2)
## code "Baked bricks and and sand" as baked bricks (5)

## code "bush" as "no toilet" (1)

## *****************************************************************
## join yield and ag data



## *****************************************************************
## random bugs???
## there is one farm that is way too big
ag$total_crop_area[ag$total_crop_area ==
                   max(ag$total_crop_area, na.rm=TRUE)] <- NA

ag$crop_div[ag$crop_div ==
            min(ag$crop_div, na.rm=TRUE)] <- NA

ag$prop_pot_labor[ag$prop_pot_labor ==
                  min(ag$prop_pot_labor, na.rm=TRUE)] <- NA


## *****************************************************************
## get classes correct


tf.cols <- c("any_div", "ext_input", "no_inputs",
                "crop_rotation", "intercrop",  "livestock_int",
                "fert_cert", "pest_herb", "inorg_fert",
                "org_fert",
                "erosion", "any_extension",
                "long_rainy_irrigation",
             "short_rainy_irrigation", "tenure",
             "long_rainy_crop_value_area",
             "short_rainy_crop_value_area",
             "long_rainy_yield_area", "short_rainy_yield_area",
             "pop_density")

for(i in tf.cols){
    print(i)
    ag[, i] <- as.numeric(ag[,i]*1)
}


made.cols.numeric <- c( "crop_div", "simpson.div",
                       "prop_education", "profit",   "profit",
                       "total_div", "prop_pot_labor",   "total_crop_area")

for(i in made.cols.numeric){
    ag[, i] <- as.numeric(ag[,i])
}


## *****************************************************************


### columns to take the mean across a HH

cols.to.av <- c("any_div", "ext_input", "no_inputs",
                "crop_rotation", "intercrop",  "livestock_int",
                "fert_cert", "pest_herb", "inorg_fert",
                "org_fert",
                "erosion",
                "long_rainy_irrigation",
                "short_rainy_irrigation",
                "total_div", "total_paid_wages", "tenure",
                "long_rainy_crop_value_area",
                "short_rainy_crop_value_area",
             "long_rainy_yield_area", "short_rainy_yield_area")

ag.hh <- aggregate(ag[,cols.to.av],
                   list(Household.ID=ag$Household.ID,
                                        Round=ag$Round),
                   mean, na.rm=TRUE)

## *****************************************************************


made.cols.factor <- c("any_div", "ext_input", "no_inputs",
                      "crop_rotation", "intercrop",  "livestock_int",
                      "fert_cert", "pest_herb", "inorg_fert",
                      "long_rainy_soil_quality",
                      "org_fert",
                      "extension",
                      "erosion",
                      "tenure",
                      "Country",  "Landscape..",
                      "long_rainy_org_fert_type",
                      "long_rainy_inorg_fert_type",
                      "short_rainy_org_fert_type",
                      "short_rainy_inorg_fert_type",
                      "long_rainy_pest_herb_type",
                      "short_rainy_pest_herb_type",
                      "water_source_rainy",
                      "water_source_dry",
                      "water_insecurity",
                      "electricity_source",
                      "lighting_fuel",
                      "cooking_fuel",
                       "long_rainy_irrigation",
                      "long_rainy_irrigation_type",
                      "short_rainy_irrigation",
                      "short_rainy_irrigation_type",
                      "long_rainy_eros_type",
                      "short_rainy_eros_type",
                      "short_rainy_field_water_source",
                      "long_rainy_field_water_source")

for(i in made.cols.factor){
    ag[, i] <- as.factor(ag[,i])
}


levels(ag$water_source_rainy) <- levels(ag$water_source_dry) <-
    c('Piped Water Inside Dwelling',
         'Private Outside Standpipe/Tap',
         'Public Standpipe/Tap',
         'Neighbouring Household',
         'Water Vendor', 'Subsidized Water Vending Station',
         'Water Truck/Tanker Service',
         'Protected Well With Pump',
         'Unprotected Well With Pump',
         'Protected Well Without Pump',
         'Unprotected Well Without Pump',
         'River, Lake, Spring, Pond',
         'Rainwater', 'Other')

levels(ag$long_rainy_pest_herb_type) <-
    levels(ag$short_rainy_pest_herb_type) <-
    c("None", 'Pesticide', 'Herbicide','Fungicide', "Other")

levels(ag$long_rainy_org_fert_type) <-  levels(ag$short_rainy_org_fert_type) <-
    c("None", 'Crop Residue', 'Animal Manure', 'Natural Fallow',
    'Leguminous Tree Fallow', 'Leguminous Cover Crop',
    'Biomass Transfer', 'Compost')

levels(ag$extension) <- c("None", 'Ministry Of Agriculture', 'Ngo',
                  'Cooperative', 'Large Scale Farmer/Outgrowers',
                  'Research Organization/University',
                  "Community Based Farmer'S Organisations (Cbos)/Farmer Based Organisations (Fbos)",
                  "other")


levels(ag$long_rainy_eros_type) <-
    levels(ag$short_rainy_eros_type) <-
    c("None", 'Terraces','Erosion Control Bunds', 'Gabions / Sandbags',
      'Vetiver Grass', 'Tree Belts', 'Water Harvest Bunds', 'Drainage Ditches', 'Dam')


## *****************************************************************

keep.same <- c("prop_education","profit","crop_div",
               "simpson.div",
               "total_crop_area", "prop_pot_labor",
               "water_source_rainy", "extension",
                       "water_source_dry",
                      "water_insecurity",
                      "electricity_source",
                      "lighting_fuel",
                      "cooking_fuel", "total_crop_rich", "hh_ind")

ag.hh <- cbind(ag.hh, ag[,keep.same][match(ag.hh$Household.ID,
                                           ag$Household.ID),])

## food insecurity

fs <- read.csv("~/Dropbox/vitalSigns/data/joinedData/FoodSecurity_HH.csv")

fs <- cbind(fs, ag.hh[match(paste0(fs$Household.ID, fs$Round),
                       paste0(ag.hh$Household.ID,
                              ag.hh$Round)),])

fs$Landscape.. <- as.factor(paste0(fs$Country, fs$Landscape..))


usethis::use_data(ag)
usethis::use_data(fs)

