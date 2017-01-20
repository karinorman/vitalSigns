# Nov. 3, 2016
# Compiling VS data

# hh data wd
setwd("~/ESPM 277/Vital Signs/Vital Signs Data/Household Survey")

# secA
secAgha<-read.csv("hh_secA_GHA_0.csv")
secArwa<-read.csv("hh_secA_RWA_0.csv")
secAtza<-read.csv("hh_secA_TZA_0.csv")
secAuga<-read.csv("hh_secA_UGA_0.csv")
hh_secA<-rbind(secAgha,secArwa,secAtza,secAuga)
write.csv(hh_secA, file = "hh_secA.csv")

# secB
secBgha<-read.csv("hh_secB_GHA_0.csv")
secBrwa<-read.csv("hh_secB_RWA_0.csv")
secBtza<-read.csv("hh_secB_TZA_0.csv")
secBuga<-read.csv("hh_secB_UGA_0.csv")
hh_secB<-rbind(secBgha,secBrwa,secBtza,secBuga)
write.csv(hh_secB, file = "hh_secB.csv")

# secC
secCgha<-read.csv("hh_secC_GHA_0.csv")
secCrwa<-read.csv("hh_secC_RWA_0.csv")
secCtza<-read.csv("hh_secC_TZA_0.csv")
secCuga<-read.csv("hh_secC_UGA_0.csv")
hh_secC<-rbind(secCgha,secCrwa,secCtza,secCuga)
write.csv(hh_secC, file = "hh_secC.csv")

# secE
secEgha<-read.csv("hh_secE_GHA_0.csv")
secErwa<-read.csv("hh_secE_RWA_0.csv")
secEtza<-read.csv("hh_secE_TZA_0.csv")
secEuga<-read.csv("hh_secE_UGA_0.csv")
hh_secE<-rbind(secEgha,secErwa,secEtza,secEuga)
write.csv(hh_secE, file = "hh_secE.csv")

# secHV1
secHV1gha<-read.csv("hh_secHV1_GHA_0.csv")
secHV1rwa<-read.csv("hh_secHV1_RWA_0.csv")
secHV1tza<-read.csv("hh_secHV1_TZA_0.csv")
secHV1uga<-read.csv("hh_secHV1_UGA_0.csv")
hh_secHV1<-rbind(secHV1gha,secHV1rwa,secHV1tza,secHV1uga)
write.csv(hh_secHV1, file = "hh_secHV1.csv")

# secHV2
secHV2gha<-read.csv("hh_secHV2_GHA_0.csv")
secHV2rwa<-read.csv("hh_secHV2_RWA_0.csv")
secHV2tza<-read.csv("hh_secHV2_TZA_0.csv")
secHV2uga<-read.csv("hh_secHV2_UGA_0.csv")
hh_secHV2<-rbind(secHV2gha,secHV2rwa,secHV2tza,secHV2uga)
write.csv(hh_secHV2, file = "hh_secHV2.csv")

# secI
secIgha<-read.csv("hh_sec_I_GHA_0.csv")
secIrwa<-read.csv("hh_sec_I_RWA_0.csv")
secItza<-read.csv("hh_sec_I_TZA_0.csv")
secIuga<-read.csv("hh_sec_I_UGA_0.csv")
hh_secI<-rbind(secIgha,secIrwa,secItza,secIuga)
write.csv(hh_secI, file = "hh_secI.csv")

# secJ1
secJ1gha<-read.csv("hh_secJ1_GHA_0.csv")
secJ1rwa<-read.csv("hh_secJ1_RWA_0.csv")
secJ1tza<-read.csv("hh_secJ1_TZA_0.csv")
secJ1uga<-read.csv("hh_secJ1_UGA_0.csv")
hh_secJ1<-rbind(secJ1gha,secJ1rwa,secJ1tza,secJ1uga)
write.csv(hh_secJ1, file = "hh_secJ1.csv")

# secJ2
secJ2gha<-read.csv("hh_secJ2_GHA_0.csv")
secJ2rwa<-read.csv("hh_secJ2_RWA_0.csv")
secJ2tza<-read.csv("hh_secJ2_TZA_0.csv")
secJ2uga<-read.csv("hh_secJ2_UGA_0.csv")
hh_secJ2<-rbind(secJ2gha,secJ2rwa,secJ2tza,secJ2uga)
write.csv(hh_secJ2, file = "hh_secJ2.csv")

# secK1
secK1gha<-read.csv("hh_secK1_GHA_0.csv")
secK1rwa<-read.csv("hh_secK1_RWA_0.csv")
secK1tza<-read.csv("hh_secK1_TZA_0.csv")
secK1uga<-read.csv("hh_secK1_UGA_0.csv")
hh_secK1<-rbind(secK1gha,secK1rwa,secK1tza,secK1uga)
write.csv(hh_secK1, file = "hh_secK1.csv")

# secK2
secK2gha<-read.csv("hh_secK2_GHA_1.csv")
secK2rwa<-read.csv("hh_secK2_RWA_1.csv")
secK2tza<-read.csv("hh_secK2_TZA_1.csv")
secK2uga<-read.csv("hh_secK2_UGA_1.csv")
hh_secK2<-rbind(secK2gha,secK2rwa,secK2tza,secK2uga)
write.csv(hh_secK2, file = "hh_secK2.csv")

# secL
sec_Lgha<-read.csv("hh_secL_GHA_0.csv")
sec_Lrwa<-read.csv("hh_secL_RWA_0.csv")
sec_Ltza<-read.csv("hh_secL_TZA_0.csv")
sec_Luga<-read.csv("hh_secL_UGA_0.csv")
hh_sec_L<-rbind(sec_Lgha,sec_Lrwa,sec_Ltza,sec_Luga)
write.csv(hh_sec_L, file = "hh_sec_L.csv")

# secN
secNgha<-read.csv("hh_secN_GHA_0.csv")
secNrwa<-read.csv("hh_secN_RWA_0.csv")
secNtza<-read.csv("hh_secN_TZA_0.csv")
secNuga<-read.csv("hh_secN_UGA_0.csv")
hh_secN<-rbind(secNgha,secNrwa,secNtza,secNuga)
write.csv(hh_secN, file = "hh_secN.csv")

# secU
secUgha<-read.csv("hh_secU_GHA_0.csv")
secUrwa<-read.csv("hh_secU_RWA_0.csv")
secUtza<-read.csv("hh_secU_TZA_0.csv")
secUuga<-read.csv("hh_secU_UGA_0.csv")
hh_secU<-rbind(secUgha,secUrwa,secUtza,secUuga)
write.csv(hh_secU, file = "hh_secU.csv")