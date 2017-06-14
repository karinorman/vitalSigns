## Nov 16
## Process hh data (after compile_data_hh_)

# setwd
setwd("~/Dropbox/vitalSigns/analysis/vital_signs/dataPrep")
data.dir <-  "../../../data/Agricultural - Household/hh_data"

# open libraries
library(dplyr)
library(lubridate)
library(ggplot2)
library(stringr)
library(stats)
library(ggfortify)


l <- read.csv(file.path(data.dir, "hh_sec_L.csv"))
a <- read.csv(file.path(data.dir, "hh_secA.csv"))
b <- read.csv(file.path(data.dir, "hh_secB.csv"))
c <- read.csv(file.path(data.dir, "hh_secC.csv"))
e <- read.csv(file.path(data.dir, "hh_secE.csv"))
hv1 <- read.csv(file.path(data.dir, "hh_secHV1.csv"))
hv2 <- read.csv(file.path(data.dir, "hh_secHV2.csv"))
i <- read.csv(file.path(data.dir, "hh_secI.csv"))
j1 <- read.csv(file.path(data.dir, "hh_secJ1.csv"))
j2 <- read.csv(file.path(data.dir, "hh_secJ2.csv"))
k1 <- read.csv(file.path(data.dir, "hh_secK1.csv"))
k2 <- read.csv(file.path(data.dir, "hh_secK2.csv"))
n <- read.csv(file.path(data.dir, "hh_secN.csv"))
u <- read.csv(file.path(data.dir, "hh_secU.csv"))

### hh roster
roster <-data.frame("country"=b$Country,
                   "hhID"=b$Household.ID,
                   "individID"=b$Individual.ID,
                   "sex"=b$hh_b02,
                   "age"=b$hh_b03)

# number of members per hh
roster_hh  <- roster %>%
  group_by(country, hhID) %>%
  summarise(number=length(individID)) %>%
    arrange(country)

unique(roster_hh$hhID)

# convert date of birth to age (in years)
age <-data.frame("country"=b$Country,
                "hhID"=b$Household.ID,
                "data_date"=b$Data.entry.date,
                "individID"=b$Individual.ID,
                "sex"=b$hh_b02,
                "dob"=b$hh_b03)
# class(age$dob)

# use lubridate to calculate age
age$dob <-as.Date(age$dob)
age$data_date <-as.Date(age$data_date)
age$year=year(age$data_date)-year(age$dob)

### number of adult males per hh
adultmen <- subset(age, year >= 18 & sex == 1)
roster_adult <- adultmen %>%
  group_by(country, hhID) %>%
  summarise(n_adult_m=length(individID)) %>%
  arrange(country)

# combine roster_hh and roster_new to calculate % of each hh that is
# adult men
roster_adultmen <- merge(x=roster_hh, y=roster_adult)
roster_adultmen$percentM <- roster_adultmen$n_adult_m/roster_adultmen$number*100
# check for duplicate hhIDs
n_occur_men <-data.frame(table(roster_adultmen$hhID))
n_occur_men[n_occur_men$Freq>1,] # no duplicates
# write.csv(roster_adultmen, file = "percent adult men.csv")

# plot % adult men per hh for each country
# menplot <-qplot(percentM, data = roster_adultmen,
#                fill=country,geom = "histogram",
#                binwidth=10)
# menplot <-menplot+labs(y="Number of Households",
#                           x="% Adult Men")+
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())+
#   scale_fill_brewer(palette = "Spectral")+
#   ylim(0,300)
# menplot

# save plot
# ggsave(file = "percentadultmen.pdf")

# plot household size
# hhsize <-ggplot(roster_adultmen,
#                aes(x=number, fill=country))
# hhsize <-hhsize+geom_bar()+
#   labs(y="Number of Households",
#        x="Size of Household")+
#   theme(panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_blank())+
#   xlim(0,20)+ylim(0,100)+
#   scale_fill_brewer(palette = "Spectral")
# hhsize

### literacy and education
# combine ages with c file
age_only <-data.frame("Country"=age$country,
                     "Household.ID"=age$hhID,
                     "Individual.ID"=age$individID,
                     "sex"=age$sex,
                     "age"=age$year)
educ <-merge(x=c, y=age_only, all = TRUE)

# data frame of number of children per hh
child <-subset(age, year < 18 & year >= 5)
roster_child <-child %>%
  group_by(country, hhID) %>%
  summarise(n_child=length(individID)) %>%
  arrange(country)

# data frame of number of adults per hh
all_adult <-subset(age, year >=18)
roster_adults <-all_adult %>%
  group_by(country, hhID) %>%
  summarise(n_adults=length(individID)) %>%
  arrange(country)

# literate adults per hh
lit_ad <-subset(educ, age >= 18 & hh_c02 < 5)
roster_lit_ad <-lit_ad %>%
  group_by(Country, Household.ID) %>%
  summarise(n_lit_ad=length(Individual.ID)) %>%
  arrange(Country)
colnames(roster_lit_ad) <-c("country", "hhID", "n_lit_ad")

# adults that have ever attended school
educ_ad <-subset(educ, age >= 18 & hh_c03==1)
roster_educ_ad <-educ_ad %>%
  group_by(Country, Household.ID) %>%
  summarise(n_educ_ad=length(Individual.ID)) %>%
  arrange(Country)
colnames(roster_educ_ad) <-c("country", "hhID", "n_educ_ad")

# literate children per hh
lit_child <-subset(educ, age < 18 & age >= 5 & hh_c02 <5)
roster_lit_ch <-lit_child %>%
  group_by(Country, Household.ID) %>%
  summarise(n_lit_ch=length(Individual.ID)) %>%
  arrange(Country)
colnames(roster_lit_ch) <-c("country", "hhID", "n_lit_ch")

# children that have ever attended school
educ_ch <-subset(educ, age < 18 & age >= 5 & hh_c03==1)
roster_educ_ch <-educ_ch %>%
  group_by(Country, Household.ID) %>%
  summarise(n_educ_ch=length(Individual.ID)) %>%
  arrange(Country)
colnames(roster_educ_ch) <-c("country", "hhID", "n_educ_ch")

# merge adult and child rosters
full_roster <-merge(x=roster_adults, y=roster_child, all = TRUE)
test <-merge(x=full_roster, y=roster_lit_ad, all = TRUE)
test1 <-merge(x=test, y=roster_educ_ad, all = TRUE)
test2 <-merge(x=test1, y=roster_lit_ch, all = TRUE)
test3 <-merge(x=test2, y=roster_educ_ch, all = TRUE)
test3[is.na(test3)] <-0
test3$percent_lit_ad <-test3$n_lit_ad/test3$n_adults*100
test3$percent_ed_ad <-test3$n_educ_ad/test3$n_adults*100
test3$percent_lit_ch <-test3$n_lit_ch/test3$n_child*100
test3$percent_ed_ch <-test3$n_educ_ch/test3$n_child*100
# write.csv(test3, file = "hh education.csv")
# check for duplicate hhIDs
n_occur_test3 <-data.frame(table(test3$hhID))
n_occur_test3[n_occur_test3$Freq>1,]
# no duplicates in test3, but need to remove rows with duplicates from other datasets
test4 <-test3[-36,]
test4 <-test4[-41,]
test4 <-test4[-427,]
test4 <-test4[-428,]
test4 <-test4[-77,]
test4 <-test4[-266,]
test4 <-test4[-457,]
test4 <-test4[-88,]
test4 <-test4[-659,]
test4 <-test4[-470,]
test4 <-test4[-301,]
test4 <-test4[-110,]
test4 <-test4[-493,]
test4 <-test4[-504,]
test4 <-test4[-695,]
test4 <-test4[-132,]
test4 <-test4[-701,]
test4 <-test4[-325,]
test4 <-test4[-539,]
test4 <-test4[-166,]
test4 <-test4[-552,]
test4 <-test4[-176,]
test4 <-test4[-184,]
test4 <-test4[-570,]

### head of household
head <-data.frame("country"=b$Country,
                   "hhID"=b$Household.ID,
                 "data_date"=b$Data.entry.date,
                   "individID"=b$Individual.ID,
                   "sex"=b$hh_b02,
                   "dob"=b$hh_b03,
                 "hhhead"=b$hh_b05)
head$dob <-as.Date(head$dob)
head$data_date <-as.Date(head$data_date)
# class(head$data_date)
head$year=year(head$data_date)-year(head$dob)
# subset to just include heads of hh
class(head$hhhead)
head <-subset(head, hhhead=1)
# dataframe of max age of head of hh
head_age <-head %>%
  group_by(country, hhID) %>%
  summarise(max_age=max(year)) %>%
  arrange(country)
# exclude hh where the head is < 18 years old
head_age <-subset(head_age, max_age>=18)
# check for duplicate hhIDs
n_occur_head <-data.frame(table(head_age$hhID))
n_occur_head[n_occur_head$Freq>1,] # no duplicates

# extract educ info from c
education <-data.frame("country"=c$Country,
                      "hhID"=c$Household.ID,
                      "individID"=c$Individual.ID,
                      "hh_c02"=c$hh_c02,
                      "hh_c03"=c$hh_c03,
                      "hh_c07"=c$hh_c07)
# exclude child heads
head_adult <-subset(head, year>=18)
# join education and head_adult
head_educ <-merge(x=head_adult, y=education, all.x = TRUE)
abc <-head_educ %>%
  group_by(country, hhID) %>%
  summarise(lit=min(hh_c02),educ=min(hh_c03)) %>%
  arrange(country)
# lit returns min value for literacy (where 5=illiterate)
# educ returns min value for ever attended school (2=no)
abc$lit[abc$lit<5] <-1
abc$lit[abc$lit==5] <-0
abc$educ[abc$educ==2] <-0
# check for duplicate hhIDs
n_occur_abc <-data.frame(table(abc$hhID))
n_occur_abc[n_occur_abc$Freq>1,]
# no duplicates in abc


### housing materials and facilities (household_secj)
## floor (j07)
# code "sand" as 1 ("earth")
j1$hh_j07[j1$hh_j07=="other"]  <- "1"

## wall (j05)
# code "sand and bricks", "bricks and sand" as mud brick (4)
j1[608, 15]=4
j1[619, 15]=4
j1[635, 15]=4
j1[672, 15]=4
j1[755, 15]=4
j1[683, 15]=4
j1[694, 15]=4
j1[738, 15]=4
# code "Mud,poles,bricks and cement" as poles and mud (2)
j1[773, 15]=2
# code "Baked bricks and and sand" as baked bricks (5)
j1[614, 15]=5

## roof (j06)
# levels(j1$hh_j06)
j1$hh_j06[j1$hh_j06=="1a"]  <- "1"

## toilet (hh_j09)
j1$toilet <-j1$hh_j09
# code "bush" as "no toilet" (1)
j1$hh_j09[j1$hh_j09=="other"] <-1

## electricity (hh_j13)
# code TORCH as NA
j1$hh_j13[j1$hh_j13=="other"] <-NA
# code NAs as 0s
j1$hh_j13[is.na(j1$hh_j13)]  <- 0

## rainy (hh_j14) and dry (hh_j15) season drinking water
# check if rainy and dry season sources=same
all(j1$hh_j14 == j1$hh_j15) # FALSE
# code "spring" and "spring water" as "spring" (12)
j1[430, 32]=12
j1[430, 34]=12
j1[432, 32]=12
j1[432, 34]=12
j1[507, 32]=12
j1[507, 34]=12
j1[564, 32]=12
j1[564, 34]=12

## water treatment: hh_j16_1
## water quality: hh_j20

### housing data
house <-data.frame("country"=j1$Country,
                  "hhID"=j1$Household.ID,
                  "hh_j01"=j1$hh_j01,
                  "hh_j04_1"=j1$hh_j04_1,
                  "hh_j05"=j1$hh_j05,
                  "hh_j06"=j1$hh_j06,
                  "hh_j07"=j1$hh_j07,
                  "hh_j09"=j1$hh_j09,
                  "hh_j13"=j1$hh_j13,
                  "hh_j14"=j1$hh_j14,
                  "hh_j15"=j1$hh_j15,
                  "hh_j16_1"=j1$hh_j16_1,
                  "hh_j20"=j1$hh_j20)
# check for duplicate hhIDs
n_occur_house <-data.frame(table(house$hhID))
n_occur_house[n_occur_house$Freq>1,]

# remove rows with duplicate hhIDs
house_clean <-house[-c(177,80,168,390,492,510,578,145,146,
                      341,446,530,166,181,758,759,434,567,
                      262,374,71,170,444,496,420,560,636,
                      752,17,21,626,744,275,316,532,576,31,
                      135,386,392,1,104,83,131,438,449),]

### fuelwood
fuelwood <-data.frame("country"=hv2$Country,
                     "hhID"=hv2$Household.ID,
                     "hh_hv109b_01"=hv2$hh_hv109b_01)
# check for duplicate hhIDs
n_occur_fuel <-data.frame(table(fuelwood$hhID))
n_occur_fuel[n_occur_fuel$Freq>1,]


### food scarcity
foodscarce <-data.frame("country"=i$Country,
                       "hhID"=i$Household.ID,
                       "hh_i08"=i$hh_i08)
# check for duplicate hhIDs
n_occur_food <-data.frame(table(foodscarce$hhID))
n_occur_food[n_occur_food$Freq>1,]

### water scarcity
waterscarce <-data.frame("country"=j2$Country,
                        "hhID"=j2$Household.ID,
                        "j20b_02"=j2$j20b_02)
# check for duplicate hhIDs
n_occur_water <-data.frame(table(waterscarce$hhID))
n_occur_water[n_occur_water$Freq>1,]

### household assets, livestock, land ownership
hhassets <-data.frame("country"=n$Country,
                     "hhID"=n$Household.ID,
                     "401"=n$n_401,
                     "402"=n$n_402,
                     "403"=n$n_403,
                     "404"=n$n_404,
                     "405"=n$n_405,
                     "406"=n$n_406,
                     "419"=n$n_419,
                     "420"=n$n_420,
                     "421"=n$n_421,
                     "422"=n$n_422,
                     "423"=n$n_423,
                     "425"=n$n_425,
                     "426"=n$n_426,
                     "427"=n$n_427,
                     "428"=n$n_428,
                     "429"=n$n_429,
                     "430"=n$n_430,
                     "434"=n$n_434,
                     "438"=n$n_438,
                     "439"=n$n_439,
                     "457"=n$n_457,
                     "458"=n$n_458,
                     "459"=n$n_459,
                     "432"=n$n_432,
                     "433"=n$n_433,
                     "435"=n$n_435,
                     "436"=n$n_436,
                     "440"=n$n_440,
                     "441"=n$n_441,
                     "442"=n$n_442,
                     "443"=n$n_443,
                     "444"=n$n_444,
                     "445"=n$n_445,
                     "446"=n$n_446,
                     "447"=n$n_447,
                     "448"=n$n_448,
                     "449"=n$n_449,
                     "450"=n$n_450,
                     "451"=n$n_451,
                     "452"=n$n_452,
                     "460"=n$n_460,
                     "461"=n$n_461,
                     "462"=n$n_462,
                     "463"=n$n_463,
                     "464"=n$n_464,
                     "465"=n$n_465,
                     "466"=n$n_466,
                     "467"=n$n_467,
                     "468"=n$n_468)
# check for duplicate hhIDs
n_occur_hhassets <-data.frame(table(hhassets$hhID))
n_occur_hhassets[n_occur_hhassets$Freq>1,]
# remove duplicates
hhassets <-hhassets[-177,]
hhassets <-hhassets[-80,]
hhassets <-hhassets[-168,]
hhassets <-hhassets[-390,]
hhassets <-hhassets[-492,]
hhassets <-hhassets[-510,]
hhassets <-hhassets[-578,]
hhassets <-hhassets[-145,]
hhassets <-hhassets[-146,]
hhassets <-hhassets[-341,]
hhassets <-hhassets[-446,]
hhassets <-hhassets[-530,]
hhassets <-hhassets[-166,]
hhassets <-hhassets[-181,]
hhassets <-hhassets[-758,]
hhassets <-hhassets[-759,]
hhassets <-hhassets[-434,]
hhassets <-hhassets[-567,]
hhassets <-hhassets[-262,]
hhassets <-hhassets[-374,]
hhassets <-hhassets[-71,]
hhassets <-hhassets[-170,]
hhassets <-hhassets[-444,]
hhassets <-hhassets[-496,]
hhassets <-hhassets[-420,]
hhassets <-hhassets[-560,]
hhassets <-hhassets[-636,]
hhassets <-hhassets[-752,]
hhassets <-hhassets[-17,]
hhassets <-hhassets[-21,]
hhassets <-hhassets[-626,]
hhassets <-hhassets[-744,]
hhassets <-hhassets[-275,]
hhassets <-hhassets[-316,]
hhassets <-hhassets[-532,]
hhassets <-hhassets[-576,]
hhassets <-hhassets[-31,]
hhassets <-hhassets[-135,]
hhassets <-hhassets[-386,]
hhassets <-hhassets[-392,]
hhassets <-hhassets[-1,]
hhassets <-hhassets[-104,]
hhassets <-hhassets[-83,]
hhassets <-hhassets[-131,]
hhassets <-hhassets[-438,]
hhassets <-hhassets[-449,]

### combine household info
# unique(data8$hhID)
# length(data1$hhID)
# nrow(roster_adultmen)
nrow(house_clean)
n_occur_house_clean <-data.frame(table(house_clean$hhID))
n_occur_house_clean[n_occur_house_clean$Freq>1,]
## join house_clean and test3
pqr <-inner_join(test3, house_clean)
# check for duplicates in pqr
n_occur_pqr <-data.frame(table(pqr$hhID))
n_occur_pqr[n_occur_pqr$Freq>1,]
## add in abc
pqr1 <-inner_join(abc, pqr)
n_occur_pqr1 <-data.frame(table(pqr1$hhID))
n_occur_pqr1[n_occur_pqr1$Freq>1,]
## add in waterscarce
pqr2 <-inner_join(waterscarce, pqr1)
n_occur_pqr2 <-data.frame(table(pqr2$hhID))
n_occur_pqr2[n_occur_pqr2$Freq>1,]
# remove 2 duplicate rows from pqr2 (161, 321)
pqr3 <-pqr2[-c(161,321),]
n_occur_pqr3 <-data.frame(table(pqr3$hhID))
n_occur_pqr3[n_occur_pqr3$Freq>1,]
## add in foodscarce
pqr4 <-inner_join(foodscarce, pqr3)
n_occur_pqr4 <-data.frame(table(pqr4$hhID))
n_occur_pqr4[n_occur_pqr4$Freq>1,]
# remove 2 duplicate rows from pqr4
pqr5 <-pqr4[-c(161,321),]
n_occur_pqr5 <-data.frame(table(pqr5$hhID))
n_occur_pqr5[n_occur_pqr5$Freq>1,]
## add in fuelwood
pqr6 <-inner_join(fuelwood, pqr5)
n_occur_pqr6 <-data.frame(table(pqr6$hhID))
n_occur_pqr6[n_occur_pqr6$Freq>1,]
# remove 2 duplicate rows
pqr7 <-pqr6[-c(161,321),]
n_occur_pqr7 <-data.frame(table(pqr7$hhID))
n_occur_pqr7[n_occur_pqr7$Freq>1,]
## add in hhassets
nrow(hhassets)
nrow(pqr7)
pqr8 <-left_join(pqr7, hhassets)
n_occur_pqr8 <-data.frame(table(pqr8$hhID))
n_occur_pqr8[n_occur_pqr8$Freq>1,]
nrow(pqr8)
# remove 1 duplicate row
pqr9 <-pqr8[-c(231),]
n_occur_pqr9 <-data.frame(table(pqr9$hhID))
n_occur_pqr9[n_occur_pqr9$Freq>1,]
nrow(pqr9)
## add in hh head age
pqr10 <-left_join(pqr9, head_age)
n_occur_pqr10 <-data.frame(table(pqr10$hhID))
n_occur_pqr10[n_occur_pqr10$Freq>1,]
## add in % adult men
pqr11 <-left_join(pqr10, roster_adultmen)
n_occur_pqr11 <-data.frame(table(pqr11$hhID))
n_occur_pqr11[n_occur_pqr11$Freq>1,]

### pqr11 has all the data!!
write.csv(pqr11, file = "hhdata_for_analysis.csv")

#########
# Dec 8.

# read in file with area owned per hh (from ag survey)
area <-read.csv("area owned per hh.csv")
area$X <-NULL
# read in csv with all other hh data
other <-read.csv("hhdata_for_analysis.csv")
other$X <-NULL
# read in livestock data
ls <-read.csv("all livestock.csv")
ls$X <-NULL
colnames(ls) <-c("country","hhID",
                "total_ls","cattle")

# merge files
all <-merge(other, area, all = TRUE)
all <-merge(all, ls, all = TRUE)
n_occur_all <-data.frame(table(all$hhID))
n_occur_all[n_occur_all$Freq>1,] # no duplicated hh's

# write.csv(all, file = "dec12.csv")

# check each column for number of NAs
# sum(is.na(all$country)) # 0
# sum(is.na(all$hh_hv109b_01)) #413: fuelwood scarcity
# sum(is.na(all$hh_i08)) # 25
# sum(is.na(all$hh_j01)) # 25
# sum(is.na(all$hh_j04_1)) # 25
# sum(is.na(all$hh_j05)) # 25
# sum(is.na(all$hh_j06)) # 25
# sum(is.na(all$hh_j07)) # 25
# sum(is.na(all$hh_j09)) # 28
# sum(is.na(all$hh_j13)) #560: main source of electricity
# remove columns with >400 NAs (fuelwood, electricity)
all$hh_hv109b_01 <-NULL
all$hh_j13 <-NULL
# sum(is.na(all$hh_j14)) # 25
# sum(is.na(all$hh_j15)) # 25
# sum(is.na(all$hh_j16_1)) # 25
# sum(is.na(all$hh_j20)) # 25
# sum(is.na(all$j20b_02)) # 62
# sum(is.na(all$lit)) # 25
# sum(is.na(all$educ)) # 25
# sum(is.na(all$n_adults)) # 25
# remove 25 rows that have consistent NA values
delete.na  <- function(DF, n=0) {
  DF[rowSums(is.na(DF)) <= n,]
}
all <-delete.na(all, 70)
# updated # of missing rows
# sum(is.na(all$hh_j09)) # 3
# sum(is.na(all$j20b_02)) # 37
# sum(is.na(all$percent_lit_ch)) # 130 (-> set to 0)
all$percent_lit_ch[is.na(all$percent_lit_ch)] <-0
all$percent_ed_ch[is.na(all$percent_ed_ch)] <-0
# sum(is.na(all$hh_j09)) # 3 (381, 504, 522)
# sum(is.na(all$X401)) # 47 rows don't have good data for hh assets
# sum(is.na(all$number)) # 63, same for # and % adult men
# sum(is.na(all$area)) # 186 don't have area
# sum(is.na(all$total_ls)) #105 rows set to NA
# drop extra livestock columns
all$X432 <-NULL
all$x433 <-NULL

# make df without assets
all_minus_assets <-delete.na(all, 30)
no_na <-all[complete.cases(all),] # 367 hh's have 0 NAs
# make matrix of all variables
no_na$n_adults <-NULL
no_na$n_child <-NULL
no_na$n_lit_ad <-NULL
no_na$n_lit_ch <-NULL
no_na$n_educ_ad <-NULL
no_na$n_educ_ch <-NULL
no_na$n_adult_m <-NULL
no_na$X433 <-NULL
no_na$max_age <-NULL

### PCA for the 367 hh's with no NA values (no_na)
## all countries together
ncol(no_na)
colnames(no_na)[colSums(is.na(no_na)) > 0]
X=as.matrix(no_na[,3:63])

# scale data
X_st <-scale(X)

# construct a sample correlation matrix S
S = cor(X_st)
colnames(S)[colSums(is.na(S))>0]
# remove variables with missing values in S
no_na$X430 <-NULL
no_na$X434 <-NULL
no_na$X443 <-NULL
no_na$X444 <-NULL
no_na$X445 <-NULL
no_na$X447 <-NULL
no_na$X448 <-NULL
no_na$X449 <-NULL
no_na$X451 <-NULL
no_na$X452 <-NULL
X=as.matrix(no_na[,3:62])

# scale data
X_st <-scale(X)

# construct a sample correlation matrix S
S = cor(X_st)

# output S to look at correlation
S_raw = cor(X)
write.csv(S_raw, file = "correlation matrix.csv")

wa <-ggplot(no_na, aes(x=hh_j14, y=hh_j15))+
             geom_point()

# drop highly correlated variables
no_na$hh_j15 <-NULL # drop dry season water source
no_na$X446 <-NULL # drop ploughs
no_na$percent_lit_ad <-NULL
no_na$percent_ed_ad <-NULL
no_na$educ <-NULL # keep literate head of hh
no_na$percent_lit_ch <-NULL
no_na$X458 <-NULL # drop chorkor over
no_na$X428 <-NULL # drop carts
no_na$X435 <-NULL #drop donkeys
no_na$X436 <-NULL # drop number of fields

X=as.matrix(no_na[,3:52])

# scale data
X_st <-scale(X)

# construct a sample correlation matrix S
S = cor(X_st)

# with prcomp
library(ggfortify)
autoplot(prcomp(no_na[,3:54], center = TRUE, scale. = TRUE),
         data = no_na,
         colour='country')

# perform PCA via spectral decomposition
fit.pca1 = eigen(S)

# get the PC directions (coefficients of linear combinations)
v1 <- fit.pca1$vectors
rownames(v1) <-colnames(X)
write.csv(v1, file = "pc_directions_all.csv")

# get the eigenvalues
evalue <-eigen(cor(X_st))$values

# get the PCs
u1 = X_st %*% v1
pc5 <-round(u1[,1:5],3)
row.names(pc5)=no_na$hhID
colnames(pc5) <-c("PC1","PC2","PC3","PC4","PC5")

# get % of variation explained
var <-round(cumsum(fit.pca1$values)/sum(fit.pca1$values),3)
variation <-data.frame(c(var))
vari <-data.frame(as.numeric(rownames(variation)),variation)
colnames(vari) <-c("PC","variation")

# plot % of variation explained
var_plot <-ggplot(vari, aes(x=PC, y=variation))+
  geom_point(shape=1)+
  ylim(0,1)+xlim(1,55)+
  labs(y="% of Variation Explained",
       x="# of Principal Components")+
  theme(text = element_text(size=10))
var_plot

# pca on hh possessions only
assets=as.matrix(no_na[,16:32])
# scale data
assets_st <-scale(assets)
# construct a sample correlation matrix S
Sassets = cor(assets_st)

# with prcomp
autoplot(prcomp(no_na[,16:32], center = TRUE, scale. = TRUE),
         data = no_na,colour='country',
         loadings = TRUE, loadings.label = TRUE)

# perform PCA via spectral decomposition
fit.pca1_assets = eigen(Sassets)

# get the PC directions (coefficients of linear combinations)
v1_assets = fit.pca1_assets$vectors
rownames(v1_assets) <-colnames(assets)
write.csv(v1_assets, file = "pc_directions_assets.csv")

# get the eigenvalues
evalue_assets <-eigen(cor(assets_st))$values

# get the PCs
u1_assets = assets_st %*% v1_assets
pc5_assets <-round(u1_assets[,1:5],3)
row.names(pc5_assets)=no_na$hhID
colnames(pc5_assets) <-c("PC1","PC2","PC3","PC4","PC5")

# get % of variation explained
var_assets <-round(cumsum(fit.pca1_assets$values)/sum(fit.pca1_assets$values),3)
variation <-data.frame(c(var))
vari <-data.frame(as.numeric(rownames(variation)),variation)
colnames(vari) <-c("PC","variation")

# plot % of variation explained
var_plot <-ggplot(vari, aes(x=PC, y=variation))+
  geom_point(shape=1)+
  ylim(0,1)+xlim(1,55)+
  labs(y="% of Variation Explained",
       x="# of Principal Components")+
  theme(text = element_text(size=10))
var_plot

### re-do PCA, without sewing machine, music system, and farm implements
no_na$X405 <-NULL # sewing
no_na$X423 <-NULL # music
no_na[,31:45] <-NULL # drop farming implements
no_na$hh_j16_1 <-NULL # drop water satisfaction

X=as.matrix(no_na[,3:34])

# scale data
X_st <-scale(X)

# construct a sample correlation matrix S
S = cor(X_st)

# with prcomp
autoplot(prcomp(no_na[,3:34], center = TRUE, scale. = TRUE),
         data = no_na,
         colour='country')

# perform PCA via spectral decomposition
fit.pca1 = eigen(S)

# get the PC directions (coefficients of linear combinations)
v1 = fit.pca1$vectors
rownames(v1) <-colnames(X_st)
write.csv(v1, file = "pc_directions_interpret2.csv")

# get the eigenvalues
evalue <-eigen(cor(X_st))$values
evalue

# get the PCs
u1 = X_st %*% v1
pc5 <-round(u1[,1:5],3)
row.names(pc5)=no_na$hhID
colnames(pc5) <-c("PC1","PC2","PC3","PC4","PC5")

# get % of variation explained
var <-round(cumsum(fit.pca1$values)/sum(fit.pca1$values),3)
variation <-data.frame(c(var))
vari <-data.frame(as.numeric(rownames(variation)),variation)
colnames(vari) <-c("PC","variation")

# plot % of variation explained
var_plot <-ggplot(vari, aes(x=PC, y=variation))+
  geom_point(shape=1)+
  ylim(0,1)+xlim(1,55)+
  labs(y="% of Variation Explained",
       x="# of Principal Components")+
  theme(text = element_text(size=10))
var_plot



### GHANA
gha <-subset(no_na, no_na$country=="GHA")

# X matrix
X_gha=as.matrix(gha[,3:34])

# scale data
X_gha_st <-scale(X_gha)

# construct a sample correlation matrix S
S_gha = cor(X_gha_st)

# perform PCA via spectral decomposition
fit.pca1_gha = eigen(S_gha)

# get the PC directions (coefficients of linear combinations)
v1_gha = fit.pca1_gha$vectors
rownames(v1_gha) <-colnames(X_gha)
write.csv(v1_gha, file = "pc_directions_ghana.csv")

# get the eigenvalues
evalue_gha <-eigen(cor(X_gha_st))$values

# get the PCs
u1_gha = X_gha_st %*% v1_gha
pc5_gha <-round(u1_gha[,1:5],3)
row.names(pc5_gha)=no_na$hhID
colnames(pc5_gha) <-c("PC1","PC2","PC3","PC4","PC5")

# get % of variation explained
var_gha <-round(cumsum(fit.pca1_gha$values)/sum(fit.pca1_gha$values),3)
variation_gha <-data.frame(c(var_gha))
vari_gha <-data.frame(as.numeric(rownames(variation_gha)),variation_gha)
colnames(vari_gha) <-c("PC","variation")

# plot % of variation explained
var_plot_gha <-ggplot(vari_gha, aes(x=PC, y=variation))+
  geom_point(shape=1)+
  ylim(0,1)+xlim(1,62)+
  labs(y="% of Variation Explained",
       x="# of Principal Components")+
  theme(text = element_text(size=10))
var_plot_gha

levels(no_na$country)

### RWANDA
rwa <-subset(no_na, no_na$country=="RWA") #98 hh's

# X matrix
X_rwa=as.matrix(rwa[,3:34])

# scale data
X_rwa_st <-scale(X_rwa)

# construct a sample correlation matrix S
S_rwa = cor(X_rwa_st)
# remove  variables that have NAs in S_rwa
rwa$hh_j01 <-NULL
rwa$X402 <-NULL
rwa$X404 <-NULL
rwa$X419 <-NULL
rwa$X421 <-NULL
rwa$X422 <-NULL
rwa$X425 <-NULL
rwa$X426 <-NULL
rwa$X429 <-NULL
rwa$X438 <-NULL
rwa$X439 <-NULL

X_rwa=as.matrix(rwa[,3:23])
X_rwa_st <-scale(X_rwa)
S_rwa = cor(X_rwa_st)

# perform PCA via spectral decomposition
fit.pca1_rwa = eigen(S_rwa)

# get the PC directions (coefficients of linear combinations)
v1_rwa = fit.pca1_rwa$vectors
rownames(v1_rwa) <-colnames(X_rwa)
write.csv(v1_rwa, file = "pc_directions_rwanda.csv")

# get the eigenvalues
evalue_rwa <-eigen(cor(X_rwa_st))$values

# get the PCs
u1_rwa = X_rwa_st %*% v1_rwa
pc5_rwa <-round(u1_rwa[,1:5],3)
row.names(pc5_rwa)=no_na$hhID
colnames(pc5_rwa) <-c("PC1","PC2","PC3","PC4","PC5")

# get % of variation explained
var_rwa <-round(cumsum(fit.pca1_rwa$values)/sum(fit.pca1_rwa$values),3)
variation_rwa <-data.frame(c(var_rwa))
vari_rwa <-data.frame(as.numeric(rownames(variation_rwa)),variation_rwa)
colnames(vari_rwa) <-c("PC","variation")

# plot % of variation explained
var_plot_rwa <-ggplot(vari_rwa, aes(x=PC, y=variation))+
  geom_point(shape=1)+
  ylim(0,1)+xlim(1,34)+
  labs(y="% of Variation Explained",
       x="# of Principal Components")+
  theme(text = element_text(size=10))
var_plot_rwa

### UGANDA
uga <-subset(no_na, no_na$country=="UGA") #126 hh's

# X matrix
X_uga=as.matrix(uga[,3:34])

# scale data
X_uga_st <-scale(X_uga)

# construct a sample correlation matrix S
S_uga = cor(X_uga_st)
# remove  variables that have NAs in S_uga
uga$X402 <-NULL
uga$X404 <-NULL
uga$X419 <-NULL
uga$X421 <-NULL
uga$X425 <-NULL
uga$X429 <-NULL
uga$X438 <-NULL
uga$X439 <-NULL

ncol(uga)
X_uga=as.matrix(uga[,3:26])
X_uga_st <- scale(X_uga)
S_uga = cor(X_uga_st)

# perform PCA via spectral decomposition
fit.pca1_uga = eigen(S_uga)

# get the PC directions (coefficients of linear combinations)
v1_uga = fit.pca1_uga$vectors
rownames(v1_uga) <- colnames(X_uga)
write.csv(v1_uga, file = "pc_directions_uganda.csv")

# get the eigenvalues
evalue_uga <- eigen(cor(X_uga_st))$values

# get the PCs
u1_uga = X_uga_st %*% v1_uga
pc5_uga <- round(u1_uga[,1:5],3)
row.names(pc5_uga)=no_na$hhID
colnames(pc5_uga) <- c("PC1","PC2","PC3","PC4","PC5")

# get % of variation explained
var_uga <- round(cumsum(fit.pca1_uga$values)/sum(fit.pca1_uga$values),3)
variation_uga <- data.frame(c(var_uga))
vari_uga <- data.frame(as.numeric(rownames(variation_uga)),variation_uga)
colnames(vari_uga) <- c("PC","variation")

# plot % of variation explained
var_plot_uga <- ggplot(vari_uga, aes(x=PC, y=variation))+
  geom_point(shape=1)+
  ylim(0,1)+xlim(1,48)+
  labs(y="% of Variation Explained",
       x="# of Principal Components")+
  theme(text = element_text(size=10))
var_plot_uga

### TANZANIA
tza <- subset(no_na, no_na$country=="TZA") #105 hh's

# X matrix
X_tza=as.matrix(tza[,3:34])

# scale data
X_tza_st <- scale(X_tza)

# construct a sample correlation matrix S
S_tza = cor(X_tza_st)
# remove  variables that have NAs in S_tza
tza$X402 <- NULL
tza$X419 <- NULL
tza$X421 <- NULL
tza$X422 <- NULL
tza$X425 <- NULL
tza$X439 <- NULL
tza$X457 <- NULL

ncol(tza)
X_tza=as.matrix(tza[,3:28])
X_tza_st <- scale(X_tza)
S_tza = cor(X_tza_st)

# perform PCA via spectral decomposition
fit.pca1_tza = eigen(S_tza)

# get the PC directions (coefficients of linear combinations)
v1_tza = fit.pca1_tza$vectors
rownames(v1_tza) <- colnames(X_tza)
write.csv(v1_tza, file = "pc_directions_tanzania.csv")

# get the eigenvalues
evalue_tza <- eigen(cor(X_tza_st))$values

# get the PCs
u1_tza = X_tza_st %*% v1_tza
pc5_tza <- round(u1_tza[,1:5],3)
row.names(pc5_tza)=no_na$hhID
colnames(pc5_tza) <- c("PC1","PC2","PC3","PC4","PC5")

# get % of variation explained
var_tza <- round(cumsum(fit.pca1_tza$values)/sum(fit.pca1_tza$values),3)
variation_tza <- data.frame(c(var_tza))
vari_tza <- data.frame(as.numeric(rownames(variation_tza)),variation_tza)
colnames(vari_tza) <- c("PC","variation")

# plot % of variation explained
var_plot_tza <- ggplot(vari_tza, aes(x=PC, y=variation))+
  geom_point(shape=1)+
  ylim(0,1)+xlim(1,53)+
  labs(y="% of Variation Explained",
       x="# of Principal Components")+
  theme(text = element_text(size=10))
var_plot_tza

### plot all variation vs number of PCs together
plot_all <- ggplot()+
  geom_point(data=vari, aes(x=PC, y=variation, color="All"))+
  geom_point(data=vari_gha, aes(x=PC, y=variation, color="Ghana"))+
  geom_point(data=vari_rwa, aes(x=PC, y=variation, color="Rwanda"))+
  geom_point(data=vari_tza, aes(x=PC, y=variation, color="Tanzania"))+
  geom_point(data=vari_uga, aes(x=PC, y=variation, color="Uganda"))+
  labs(y="% of Variation Explained",
       x="# of Principal Components")+
  theme(legend.title=element_blank())
plot_all

var_plot <- ggplot(vari, aes(x=PC, y=variation))+
  geom_point(shape=1)+
  ylim(0,1)+xlim(1,62)+
  labs(y="% of Variation Explained",
       x="# of Principal Components")+
  theme(text = element_text(size=10))
var_plot


### CLUSTER ANALYSIS
cl <- no_na
row.names(cl) <- no_na$hhID
cl$hhID <- NULL
hca <- hclust(dist(cl[,2:33]),"average")
plot(hca, hang=-1, main=NULL,
     ylab=NULL,cex=0.7, xlab = NULL, labels=FALSE) # 5 clusters
# playing around
labelColors = c("#CC0033", "#3300CC", "#666666",
                "#33CC33", "#9900CC", "#3399FF",
                "#000000","#FFCC00","#CCCCCC","#CC66FF")
hcad=as.dendrogram(hca)
plot(hcad)
clusMember=cutree(hca, 10)
colLab  <-  function(n) {
  if (is.leaf(n)) {
    a  <-  attributes(n)
    labCol  <-  labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar")  <-  c(a$nodePar, lab.col = labCol)
  }
  n
}
clusDendro = dendrapply(hcad, colLab)

# k means clustering
km <- kmeans(cl[,2:33], 10)
