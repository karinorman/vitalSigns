## Nov. 16, 2016
setwd("~/ESPM 277/Vital Signs/Vital Signs Data/Agricultural Management Intensity Survey")

library(dplyr)

a<-read.csv("agr_secA.csv")
extfam<-read.csv("agr_sec12a_extension_family.csv")
ext<-read.csv("agr_sec12_extension.csv")
imp<-read.csv("agr_sec11_implements.csv")
liveby<-read.csv("agr_sec10b_livestock_products.csv")
live<-read.csv("agr_sec10a_livestock.csv")
livefield<-read.csv("agr_sec10_livestock_by_field.csv")
bypro<-read.csv("agr_sec9_byproducts.csv")
permcropcrop<-read.csv("agr_sec7_permanent_crops_by_crop.csv")
permcropfield<-read.csv("agr_sec6_permanent_crops_by_field.csv")
crophh<-read.csv("agr_sec5_crops_by_hh.csv")
cropfield<-read.csv("agr_sec4_crops_by_field.csv")
fieldlabor<-read.csv("agr_sec_3_7_field_details_labor.csv")
fielddet<-read.csv("agr_sec_3_7_field_details.csv")
fieldroster<-read.csv("agr_sec_2_field_roster.csv")
memroster<-read.csv("agr_sec_1_member_roster.csv")

### 
## columns of interest from fielddet: hhID, country, field ID, ag3a_07, ag3a_14, ag3a_18, ag3a_23, ag3a_33
## number of hhs and fields in fielddet and cropfield
unique(fielddet$Household.ID) # 746
unique(cropfield$Household.ID) # 743
# vector of hhIDs from each dataset
fielddet_hh<-data.frame(x=fielddet$Household.ID)
cropfield_hh<-data.frame(y=cropfield$Household.ID)
subset(fielddet_hh, !(x %in% cropfield_hh$y))
# households in field_details but not in crops_by_field: TEgeq116YW6, smz4XRTY7G6, Ce8hTSBH8m9
# subset fielddet to exclude those 5 households
fielddet<-subset(fielddet, Household.ID!="TEgeq116YW6")
fielddet<-subset(fielddet, Household.ID!="smz4XRTY7G6")
fielddet<-subset(fielddet, Household.ID!="Ce8hTSBH8m9")

# check for duplicate rows in fielddet (field/hhId duplicates)
fieldcheck<-data.frame("fieldID"=fielddet$Field.ID, "hhID"=fielddet$Household.ID)
duplicated(fieldcheck)
dup<-fieldcheck[duplicated(fieldcheck),]
dup_mat<-data.frame(as.numeric(rownames(dup)),dup)
dup_vector<-dup_mat[,1]
fielddet_nodup<-fielddet[ ! fielddet$X %in% dup_vector,]
# duplicated sets of fields and hhIDs removed!

# check for duplicate rows in cropfield
cropcheck<-data.frame("fieldID"=cropfield$Field.ID, "hhID"=cropfield$Household.ID)
duplicated(cropcheck)
dupc<-cropcheck[duplicated(cropcheck),]
dupc_mat<-data.frame(as.numeric(rownames(dupc)),dupc)
dupc_vector<-dupc_mat[,1]
cropfield_nodup<-cropfield[ ! cropfield$X %in% dupc_vector,]
# duplicated sets of fields and hhIDs removed!

# reasons for intercropping
levels(cropfield_nodup$ag4a_05)
sum(cropfield_nodup$ag4a_05=="1") # 159
sum(cropfield_nodup$ag4a_05=="2") # 55
sum(cropfield_nodup$ag4a_05=="2a") # 156
sum(cropfield_nodup$ag4a_05=="other") # 11
levels(cropfield_nodup$ag4a_04)
cropfield_nodup$ag4a_04<-as.factor(cropfield_nodup$ag4a_04)
table(cropfield_nodup$ag4a_04) # 383 use intercropping
55/383

## calculate # fields per hh for the 2
fielddet_fields<- fielddet_nodup %>%
  group_by(Household.ID, Country) %>%
  summarise(nfields_x=n_distinct(Field.ID)) %>%
  arrange(Household.ID)
cropfield_fields<- cropfield_nodup %>%
  group_by(Household.ID, Country) %>%
  summarise(nfields_y=n_distinct(Field.ID)) %>%
  arrange(Household.ID)
all<-merge(fielddet_fields, cropfield_fields, all = TRUE)

# drop X columns
cropfield_nodup$X<-NULL
fielddet_nodup$X<-NULL

cf_keep<-data.frame("country"=cropfield_nodup$Country,
                    "hhID"=cropfield_nodup$Household.ID,
                    "fieldID"=cropfield_nodup$Field.ID,
                    "intercrop"=cropfield_nodup$ag4a_04)
fd_keep<-data.frame("country"=fielddet_nodup$Country,
                    "hhID"=fielddet_nodup$Household.ID,
                    "fieldID"=fielddet_nodup$Field.ID,
                    "erosion"=fielddet_nodup$ag3a_07,
                    "tenure"=fielddet_nodup$ag3a_14,
                    "org fert"=fielddet_nodup$ag3a_18,
                    "inorg fert"=fielddet_nodup$ag3a_23,
                    "pest/herb"=fielddet_nodup$ag3a_33)

# merge dataframes
cf<-left_join(cf_keep, fd_keep)
complete<-cf[complete.cases(cf),] #1108 fields
# sum(complete$country=="GHA") # 334
# sum(complete$country=="RWA") # 214
# sum(complete$country=="TZA") # 332
# sum(complete$country=="UGA") # 228
complete<-complete[,c(1,2,3,6,4,5,7,8,9)] # reorder columns
# write.csv(complete, file="farm_practices.csv")

# simplify tenure classes
# set "shared-own" equal to "owned" (1)
complete$tenure[complete$tenure==5]<-1
# set non-ownership categories equal to 0
complete$tenure[complete$tenure>1]<-0

## correlation matrix for farming practices
corr<-complete
corr$intercrop[corr$intercrop=="2"]<-0
corr$erosion[corr$erosion=="2"]<-0
corr$org.fert[corr$org.fert=="2"]<-0
corr$inorg.fert[corr$inorg.fert=="2"]<-0
corr$pest.herb[corr$pest.herb=="2"]<-0
Xcor<-data.matrix(corr[,5:9])

Scor=cor(Xcor)


## logistic regression on complete2
# matrix of farming practices
X<-data.matrix(complete[,5:9])
X[X=="2"]<-0
X1<-as.data.frame(X)

# vector of tenure
Y<-complete[,4]

# number of owned fields
length(which(Y > 0)) #957

# set logistic reg model
fit.glm<-glm(Y~X, data = complete, family = "binomial")
summary(fit.glm)
exp(coef(fit.glm))

# model performance
n=nrow(complete)
yb.hat<-rep(0,n)
yb.hat[fitted(fit.glm)>0.5]<-1
sum(complete$tenure !=yb.hat)/length(complete$tenure)

# each country alone
# GHA
gha<-subset(complete, country=="GHA")
# matrix of farming practices
X_g<-data.matrix(gha[,5:9])
X_g[X_g=="2"]<-0
X1_g<-as.data.frame(X_g)

# vector of tenure
Y_g<-gha[,4]

# number of owned fields
length(which(Y_g > 0)) #250

# set logistic reg model
fit.glm_g<-glm(Y_g~X_g, data = gha, family = "binomial")
summary(fit.glm_g)
exp(coef(fit.glm_g))

# model performance
n=nrow(gha)
yb.hat_g<-rep(0,n)
yb.hat_g[fitted(fit.glm_g)>0.5]<-1
sum(gha$tenure !=yb.hat_g)/length(gha$tenure)

# RWA
rwa<-subset(complete, country=="RWA")
# matrix of farming practices
X_r<-data.matrix(rwa[,5:9])
X_r[X_r=="2"]<-0
X1_r<-as.data.frame(X_r)

# vector of tenure
Y_r<-rwa[,4]

# number of owned fields
length(which(Y_r > 0)) #249

# set logistic reg model
fit.glm_r<-glm(Y_r~X_r, data = rwa, family = "binomial")
summary(fit.glm_r)
exp(coef(fit.glm_r))

# model performance
n=nrow(rwa)
yb.hat_r<-rep(0,n)
yb.hat_r[fitted(fit.glm_r)>0.5]<-1
sum(rwa$tenure !=yb.hat_r)/length(rwa$tenure)

# TZA
tza<-subset(complete, country=="TZA")
# matrix of farming practices
X_t<-data.matrix(tza[,5:9])
X_t[X_t=="2"]<-0
X1_t<-as.data.frame(X_t)

# vector of tenure
Y_t<-tza[,4]

# number of owned fields
length(which(Y_t > 0)) #133

# set logistic reg model
fit.glm_t<-glm(Y_t~X_t, data = tza, family = "binomial")
summary(fit.glm_t)
exp(coef(fit.glm_t))

# model performance
n=nrow(tza)
yb.hat_t<-rep(0,n)
yb.hat_t[fitted(fit.glm_t)>0.5]<-1
sum(tza$tenure !=yb.hat_t)/length(tza$tenure)

## UGA
uga<-subset(complete, country=="UGA")
# matrix of farming practices
X_u<-data.matrix(uga[,5:9])
X_u[X_u=="2"]<-0
X1_u<-as.data.frame(X_u)

# vector of tenure
Y_u<-uga[,4]

# number of owned fields
length(which(Y_u > 0)) #216

# set logistic reg model
fit.glm_u<-glm(Y_u~X_u, data = uga, family = "binomial")
summary(fit.glm_u)
exp(coef(fit.glm_u))

# model performance
n=nrow(uga)
yb.hat_u<-rep(0,n)
yb.hat_u[fitted(fit.glm_u)>0.5]<-1
sum(uga$tenure !=yb.hat_u)/length(uga$tenure)

## MANOVA on farming practices
class(complete$tenure)
complete$tenure<-as.factor(complete$tenure)
# make matrix of ag practices (response variables)
x <- as.matrix(complete[, 5:9])

# make vector of the independent variable (tenure)
tenure_vector <- complete$tenure

# fit manova
tenure_manova <- manova(cbind(intercrop, erosion,
                              org.fert, inorg.fert,
                              pest.herb) ~ tenure_vector, 
                        data=complete)

# fit MANOVA to see if tenure affects ag practices
summary(tenure_manova, test = "Wilks")

## ghana
ghana<-subset(complete, complete$country=="GHA")
# make matrix of ag practices (response variables)
xg <- as.matrix(gha[, 5:9])

# make vector of the independent variable (tenure)
tenure_vectorg <- gha$tenure

# fit manova
tenure_manovag <- manova(cbind(intercrop, erosion,
                              org.fert, inorg.fert,
                              pest.herb) ~ tenure_vectorg, 
                        data=gha)

# fit MANOVA to see if tenure affects ag practices
summary(tenure_manovag, test = "Wilks")

## rwa
rwanda<-subset(complete, complete$country=="RWA")
# make matrix of ag practices (response variables)
xr <- as.matrix(rwa[, 5:9])

# make vector of the independent variable (tenure)
tenure_vectorr <- rwa$tenure

# fit manova
tenure_manovar <- manova(cbind(intercrop, erosion,
                               org.fert, inorg.fert,
                               pest.herb) ~ tenure_vectorr, 
                         data=rwa)

# fit MANOVA to see if tenure affects ag practices
summary(tenure_manovar, test = "Wilks")

## tza
tanz<-subset(complete, complete$country=="TZA")
# make matrix of ag practices (response variables)
xt <- as.matrix(tanz[, 5:9])

# make vector of the independent variable (tenure)
tenure_vectort <- tanz$tenure

# fit manova
tenure_manovat <- manova(cbind(intercrop, erosion,
                               org.fert, inorg.fert,
                               pest.herb) ~ tenure_vectort, 
                         data=tanz)

# fit MANOVA to see if tenure affects ag practices
summary(tenure_manovat, test = "Wilks")

## uga
ug<-subset(complete, complete$country=="UGA")
# make matrix of ag practices (response variables)
xu <- as.matrix(ug[, 5:9])

# make vector of the independent variable (tenure)
tenure_vectoru <- ug$tenure

# fit manova
tenure_manovau <- manova(cbind(intercrop, erosion,
                               org.fert, inorg.fert,
                               pest.herb) ~ tenure_vectoru, 
                         data=ug)

# fit MANOVA to see if tenure affects ag practices
summary(tenure_manovau, test = "Wilks")

tapply(complete$intercrop, complete$tenure, mean)
tapply(complete$erosion, complete$tenure, mean)
tapply(complete$org.fert, complete$tenure, mean)
tapply(complete$inorg.fert, complete$tenure, mean)
tapply(complete$pest.herb, complete$tenure, mean)


### summaries
inter=sum(complete$intercrop==1)
eros=sum(complete$erosion==1)
org=sum(complete$org.fert==1)
inorg=sum(complete$inorg.fert==1)
pest=sum(complete$pest.herb==1)
owned=sum(complete$tenure==1)
total=nrow(complete)
all_all <- data.frame(inter, eros, org, inorg, pest, owned, total)
row.names(all_all)<-c("all")
# gha
ginter=sum(gha$intercrop==1)
geros=sum(gha$erosion==1)
gorg=sum(gha$org.fert==1)
ginorg=sum(gha$inorg.fert==1)
gpest=sum(gha$pest.herb==1)
gowned=sum(gha$tenure==1)
gtotal=nrow(gha)
gall_all <- data.frame(ginter, geros, gorg, ginorg, gpest, gowned, gtotal)
colnames(gall_all)<-c("inter", "eros", "org", "inorg", "pest", "owned", "total")
row.names(gall_all)<-c("ghana")
# rwa
rinter=sum(rwa$intercrop==1)
reros=sum(rwa$erosion==1)
rorg=sum(rwa$org.fert==1)
rinorg=sum(rwa$inorg.fert==1)
rpest=sum(rwa$pest.herb==1)
rowned=sum(rwa$tenure==1)
rtotal=nrow(rwa)
rall_all <- data.frame(rinter, reros, rorg, rinorg, rpest, rowned, rtotal)
colnames(rall_all)<-c("inter", "eros", "org", "inorg", "pest", "owned", "total")
row.names(rall_all)<-c("rwanda")
# tanz
tinter=sum(tanz$intercrop==1)
teros=sum(tanz$erosion==1)
torg=sum(tanz$org.fert==1)
tinorg=sum(tanz$inorg.fert==1)
tpest=sum(tanz$pest.herb==1)
towned=sum(tanz$tenure==1)
ttotal=nrow(tanz)
tall_all <- data.frame(tinter, teros, torg, tinorg, tpest, towned, ttotal)
colnames(tall_all)<-c("inter", "eros", "org", "inorg", "pest", "owned", "total")
row.names(tall_all)<-c("tanz")
# uga
uinter=sum(uga$intercrop==1)
ueros=sum(uga$erosion==1)
uorg=sum(uga$org.fert==1)
uinorg=sum(uga$inorg.fert==1)
upest=sum(uga$pest.herb==1)
uowned=sum(uga$tenure==1)
utotal=nrow(uga)
uall_all <- data.frame(uinter, ueros, uorg, uinorg, upest, uowned, utotal)
colnames(uall_all)<-c("inter", "eros", "org", "inorg", "pest", "owned", "total")
row.names(uall_all)<-c("uganda")
all_all<-rbind(all_all, gall_all, rall_all, tall_all, uall_all)
all_all$p_inter<-all_all$inter/all_all$total*100
all_all$p_eros<-all_all$eros/all_all$total*100
all_all$p_org<-all_all$org/all_all$total*100
all_all$p_inorg<-all_all$inorg/all_all$total*100
all_all$p_pest<-all_all$pest/all_all$total*100
all_all<-round(all_all, 2)
rownames(all_all)<-c("ALL","GHA","RWA","TAN","UGA")
library(reshape2)
all_all$country<-row.names(all_all)
pall_all<-all_all[,8:13]
colnames(pall_all)<-c("INT","ERO","ORG",
                      "INORG","PEST","Country")
m_all_all<-melt(pall_all)


## types of fertilizer, etc
pr_det<-data.frame("country"=fielddet_nodup$Country,
                    "hhID"=fielddet_nodup$Household.ID,
                    "fieldID"=fielddet_nodup$Field.ID,
                    "erosion"=fielddet_nodup$ag3a_07,
                   "eros_type"=fielddet_nodup$ag3a_08_1,
                    "org fert"=fielddet_nodup$ag3a_18,
                   "org fert_type"=fielddet_nodup$ag3a_18b,
                    "inorg fert"=fielddet_nodup$ag3a_23,
                   "inorg fert_type"=fielddet_nodup$ag3a_24,
                    "pest/herb"=fielddet_nodup$ag3a_33,
                   "pest/herb_type"=fielddet_nodup$ag3a_34)
# remove rows with NAs in erosion
write.csv(pr_det, file = "practices_det.csv") # changed NAs in conditional columns to 0s
pr_det2<-read.csv("practices_det.csv")
pr_det2$erosion[is.na(pr_det2$erosion)] <- 0
erosion_x<-subset(pr_det, erosion>0) #1233 fields with erosion data
erosion_x$eros_type[is.na(erosion_x$eros_type)]<-0
# sum(erosion_x$eros_type==1)
# sum(erosion_x$eros_type==2)
# sum(erosion_x$eros_type==3)
# sum(erosion_x$eros_type==4)
# sum(erosion_x$eros_type==5)
# sum(erosion_x$eros_type==6)
# sum(erosion_x$eros_type==7)
# sum(erosion_x$eros_type==8)
sum(erosion_x$eros_type==0)
# org fert
pr_det2$org.fert[is.na(pr_det2$org.fert)] <- 0
orgf_x<-subset(pr_det, org.fert>0)#1924
orgf_x$org.fert_type[is.na(orgf_x$org.fert_type)]<-0
sum(orgf_x$org.fert_type==1)
sum(orgf_x$org.fert_type==2)
sum(orgf_x$org.fert_type==3)
sum(orgf_x$org.fert_type==4)
sum(orgf_x$org.fert_type==5)
sum(orgf_x$org.fert_type==6)
sum(orgf_x$org.fert_type==7)
sum(orgf_x$org.fert_type==8)
sum(orgf_x$org.fert_type==0)
# inorg fert
pr_det2$inorg.fert[is.na(pr_det2$inorg.fert)] <- 0
inorgf_x<-subset(pr_det, inorg.fert>0) #1924
class(inorgf_x$inorg.fert_type)
inorgf_x$inorg.fert_type<-as.character(inorgf_x$inorg.fert_type)
inorgf_x$inorg.fert_type[inorgf_x$inorg.fert_type==""]<-0
inorgf_x$inorg.fert_type<-as.factor(inorgf_x$inorg.fert_type)
sum(inorgf_x$inorg.fert_type==0)
levels(inorgf_x$inorg.fert_type)
sum(inorgf_x$inorg.fert_type=="CAN")
sum(inorgf_x$inorg.fert_type=="DAP")
sum(inorgf_x$inorg.fert_type=="MRP")
sum(inorgf_x$inorg.fert_type=="NPK")
sum(inorgf_x$inorg.fert_type=="other")
sum(inorgf_x$inorg.fert_type=="SA")
sum(inorgf_x$inorg.fert_type=="TSP")
sum(inorgf_x$inorg.fert_type=="UREA")
# pest
pr_det2$pest.herb[is.na(pr_det2$pest.herb)] <- 0
pest_x<-subset(pr_det, pest.herb>0) #1920
levels(pest_x$pest.herb_type)
pest_x$pest.herb_type<-as.character(pest_x$pest.herb_type)
pest_x$pest.herb_type[pest_x$pest.herb_type==""]<-0
pest_x$pest.herb_type<-as.factor(pest_x$pest.herb_type)
pest_x$pest.herb_type[is.na(pest_x$pest.herb_type)]<-0
sum(pest_x$pest.herb_type==1)
sum(pest_x$pest.herb_type==2)
sum(pest_x$pest.herb_type==3)
sum(pest_x$pest.herb_type=="other")
sum(pest_x$pest.herb_type==0)

# plot
fpr<-ggplot(m_all_all, aes(x=variable, y=value, fill=Country))+
  geom_bar(stat="identity", position = position_dodge())+
  scale_fill_brewer(palette="Spectral")+
  ylab("Percent of Farms")+xlab("Farming Practice")+ 
  theme_bw() + theme(panel.border = element_blank())
fpr

# plot erosion practices, etc
agp<-read.csv("ag_practices2.csv")
colnames(agp)<-c("practice","type","number","percent")
agp_p<-ggplot(agp, aes(x=type, y=percent))+
  geom_bar(stat = "identity")
agp_p


### t-tests on tenure
ten_complete<-subset(complete, complete$tenure==1)
noten_complete<-subset(complete, complete$tenure==0)
a1<-sum(ten_complete$intercrop==1)
a2<-sum(ten_complete$erosion==1)
a3<-sum(ten_complete$org.fert==1)
a4<-sum(ten_complete$inorg.fert==1)
a5<-sum(ten_complete$pest.herb==1)
b1<-sum(noten_complete$intercrop==1)
b2<-sum(noten_complete$erosion==1)
b3<-sum(noten_complete$org.fert==1)
b4<-sum(noten_complete$inorg.fert==1)
b5<-sum(noten_complete$pest.herb==1)
c1<-sum(ten_complete$intercrop==2)
c2<-sum(ten_complete$erosion==2)
c3<-sum(ten_complete$org.fert==2)
c4<-sum(ten_complete$inorg.fert==2)
c5<-sum(ten_complete$pest.herb==2)
d1<-sum(noten_complete$intercrop==2)
d2<-sum(noten_complete$erosion==2)
d3<-sum(noten_complete$org.fert==2)
d4<-sum(noten_complete$inorg.fert==2)
d5<-sum(noten_complete$pest.herb==2)

# tenure & inter
t1=c(a1,c1)
t0=c(b1,d1)
t_int<-as.data.frame(rbind(t1,t0))
rownames(t_int)<-c("own","not owned")
colnames(t_int)<-c("intercropped","did not inter")
chisq.test(t_int)

# tenure & erosion
e1=c(a2,c2)
e0=c(b2,d2)
e_int<-as.data.frame(rbind(e1,e0))
rownames(e_int)<-c("own","not owned")
colnames(e_int)<-c("eros cont","no eros cont")
chisq.test(e_int)

# tenure & org fert
o1=c(a3,c3)
o0=c(b3,d3)
o_int<-as.data.frame(rbind(o1,o0))
rownames(o_int)<-c("own","not owned")
colnames(o_int)<-c("orgf","no orgf")
chisq.test(o_int)

# tenure & inorg fert
io1=c(a4,c4)
io0=c(b4,d4)
io_int<-as.data.frame(rbind(io1,io0))
rownames(io_int)<-c("own","not owned")
colnames(io_int)<-c("inorgf","no inorgf")
chisq.test(io_int)

# tenure & inorg fert
p1=c(a5,c5)
p0=c(b5,d5)
p_int<-as.data.frame(rbind(p1,p0))
rownames(p_int)<-c("own","not owned")
colnames(p_int)<-c("pest","no pest")
chisq.test(p_int)


### Does the household own any land?
### area of land owned
own<-data.frame("country"=fielddet_nodup$Country,
                "hhID"=fielddet _nodup$Household.ID,
                "fieldID"=fielddet_nodup$Field.ID,
                "tenure"=fielddet_nodup$ag3a_14)

fieldroster<-read.csv("agr_sec_2_field_roster.csv")
# check for duplicate rows in fieldroster (field/hhId duplicates)
fieldroster_check<-data.frame("fieldID"=fieldroster$Field.ID, "hhID"=fieldroster$Household.ID)
duplicated(fieldroster_check)
dup_fr<-fieldroster_check[duplicated(fieldroster_check),]
dupfr_mat<-data.frame(as.numeric(rownames(dup_fr)),dup_fr)
dupfr_vector<-dupfr_mat[,1]
fieldroster_nodup<-fieldroster[ ! fieldroster$X %in% dupfr_vector,]

# extract info relevant to field area
fieldarea<-data.frame("country"=fieldroster_nodup$Country,
                      "hhID"=fieldroster_nodup$Household.ID,
                      "fieldID"=fieldroster_nodup$Field.ID,
                      "fieldarea"=fieldroster_nodup$ag2a_04)
area_own<-merge(fieldarea, own, all = TRUE)

# export as csv
write.csv(area_own, file = "area_owned.csv")

# area of owned fields by hh
# set "shared-own" equal to "owned" (1)
area_own$tenure[is.na(area_own$tenure)]<-0
area_own$tenure[area_own$tenure==5]<-1
area_own[1353,5]<-1
# set non-ownership categories equal to 0
area_own$tenure[area_own$tenure>1]<-0

# calculate area owned per hh
owned<-subset(area_own, area_own$tenure>0)
owned$fieldarea[is.na(owned$fieldarea)]<-0
area_owned<- owned %>%
  group_by(hhID, country) %>%
  summarise(area=sum(fieldarea)) %>%
  arrange(hhID)
write.csv(area_owned, file = "area owned per hh.csv")

# plot area owned per hh
library(ggplot2)
areaplot<-ggplot(area_owned, aes(x=area, fill=country))
areaplot<-areaplot+geom_bar()+
  labs(y="Number of Households", x="Area of Land Owned")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

areaplot2<-ggplot(area_owned, aes(area))+
  geom_freqpoly(aes(group=country, colour=country))+
  scale_fill_brewer(palette="Spectral")+
  labs(y="Number of Households", x="Area of Farmland Owned (acres)")+
  theme(legend.title=element_blank())

### livestock
# figure out how many surveyed hh had livestock
a_hh<-a$Household.ID
a_hh_unqiue<-unique(a_hh) # 743
missing<-l[! l$Household.ID %in% a_hh_unqiue,]
# 17 hhIDs in livestock data but not ag roster
l_hh<-l$Household.ID
l_hh_unique<-unique(l_hh)
missing2<-a[! a$Household.ID %in% l_hh_unique,]
# 15 hhIDs without livestock data--> assume these have no livestock ("missing2")

# livestock per hh
livest<- live %>%
  group_by(Country, Household.ID, Livestock.code) %>%
  summarise(number_live=sum(ag10a_05_1)) %>%
  arrange(Household.ID)

# group cattle
bull<-subset(livest, livest$Livestock.code=="1")
cow<-subset(livest, livest$Livestock.code=="2")
steer<-subset(livest, livest$Livestock.code=="3")
hei<-subset(livest, livest$Livestock.code=="4")
mc<-subset(livest, livest$Livestock.code=="5")
fc<-subset(livest, livest$Livestock.code=="6")
ox<-subset(livest, livest$Livestock.code=="15a")
cows<-merge(bull, cow, all = TRUE)
cows<-merge(cows, steer, all = TRUE)
cows<-merge(cows, hei, all = TRUE)
cows<-merge(cows, mc, all = TRUE)
cows<-merge(cows, fc, all = TRUE)
cows<-merge(cows, ox, all = TRUE)

# number of cattle per hh
cows[is.na(cows)]<-0
cattle<- cows %>%
  group_by(Country, Household.ID) %>%
  summarise(number_cows=sum(number_live)) %>%
  arrange(Household.ID)

# total # livestock per hh
livest[is.na(livest)]<-0
livehh<- livest %>%
  group_by(Country, Household.ID) %>%
  summarise(total=sum(number_live)) %>%
  arrange(Household.ID)

# combine cattle and total livestock
all_live<-left_join(livehh, cattle)
all_live[is.na(all_live)]<-0
# read in missing rows
live_missing<-read.csv("livestock_hh.csv")
colnames(live_missing)<-c("Country",
                          "Household.ID",
                          "total",
                          "number_cows")
all_livest<-rbind(all_live, live_missing)
?rbind
write.csv(all_livest, file = "all livestock.csv")
