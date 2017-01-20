# Nov. 3, 2016
# Compiling VS data

# wd for Ag Management Intensity Survey
setwd("~/ESPM 277/Vital Signs/Vital Signs Data/Agricultural Management Intensity Survey")

# sec1
sec1gha<-read.csv("agric_sec1_member_roster_GHA_0.csv")
sec1rwa<-read.csv("agric_sec1_member_roster_RWA_0.csv")
sec1tza<-read.csv("agric_sec1_member_roster_TZA_0.csv")
sec1uga<-read.csv("agric_sec1_member_roster_UGA_0.csv")
agr_sec1_member_roster<-rbind(sec1gha,sec1rwa,sec1tza,sec1uga)
write.csv(agr_sec1_member_roster, file = "agr_sec_1_member_roster.csv")

# sec2
sec2gha<-read.csv("agric_sec2_field_roster_GHA_0.csv")
sec2rwa<-read.csv("agric_sec2_field_roster_RWA_0.csv")
sec2tza<-read.csv("agric_sec2_field_roster_TZA_0.csv")
sec2uga<-read.csv("agric_sec2_field_roster_UGA_0.csv")
agr_sec2_member_roster<-rbind(sec2gha,sec2rwa,sec2tza,sec2uga)
write.csv(agr_sec2_member_roster, file = "agr_sec_2_field_roster.csv")

# sec3_7
sec3_7gha<-read.csv("agric_sec3_7_field_details_labor_GHA_1.csv")
sec3_7rwa<-read.csv("agric_sec3_7_field_details_labor_RWA_1.csv")
sec3_7tza<-read.csv("agric_sec3_7_field_details_labor_TZA_1.csv")
sec3_7uga<-read.csv("agric_sec3_7_field_details_labor_UGA_1.csv")
agr_sec3_7_field_details_labor<-rbind(sec3_7gha,sec3_7rwa,sec3_7tza,sec3_7uga)
write.csv(agr_sec3_7_field_details_labor, 
          file = "agr_sec_3_7_field_details_labor.csv")

# sec3
sec3gha<-read.csv("agric_sec3_field_details_GHA_0.csv")
sec3rwa<-read.csv("agric_sec3_field_details_RWA_0.csv")
sec3tza<-read.csv("agric_sec3_field_details_TZA_0.csv")
sec3uga<-read.csv("agric_sec3_field_details_UGA_0.csv")
agr_sec3_field_details<-rbind(sec3gha,sec3rwa,sec3tza,sec3uga)
write.csv(agr_sec3_field_details, 
          file = "agr_sec_3_7_field_details.csv")

# sec4
sec4gha<-read.csv("agric_sec4_crops_by_field_GHA_0.csv")
sec4rwa<-read.csv("agric_sec4_crops_by_field_RWA_0.csv")
sec4tza<-read.csv("agric_sec4_crops_by_field_TZA_0.csv")
sec4uga<-read.csv("agric_sec4_crops_by_field_UGA_0.csv")
agr_sec4_crops_by_field<-rbind(sec4gha,sec4rwa,sec4tza,sec4uga)
write.csv(agr_sec4_crops_by_field, 
          file = "agr_sec4_crops_by_field.csv")

# sec5
sec5gha<-read.csv("agric_sec5_crops_by_hh_GHA_0.csv")
sec5rwa<-read.csv("agric_sec5_crops_by_hh_RWA_0.csv")
sec5tza<-read.csv("agric_sec5_crops_by_hh_TZA_0.csv")
sec5uga<-read.csv("agric_sec5_crops_by_hh_UGA_0.csv")
agr_sec5_crops_by_hh<-rbind(sec5gha,sec5rwa,sec5tza,sec5uga)
write.csv(agr_sec5_crops_by_hh, 
          file = "agr_sec5_crops_by_hh.csv")

# sec6
sec6gha<-read.csv("agric_sec6_permanent_crops_by_field_GHA_0.csv")
sec6rwa<-read.csv("agric_sec6_permanent_crops_by_field_RWA_0.csv")
sec6tza<-read.csv("agric_sec6_permanent_crops_by_field_TZA_0.csv")
sec6uga<-read.csv("agric_sec6_permanent_crops_by_field_UGA_0.csv")
agr_sec6_permanent_crops_by_field<-rbind(sec6gha,sec6rwa,sec6tza,sec6uga)
write.csv(agr_sec6_permanent_crops_by_field, 
          file = "agr_sec6_permanent_crops_by_field.csv")

# sec7
sec7gha<-read.csv("agric_sec7_permanent_crops_by_crop_GHA_0.csv")
sec7rwa<-read.csv("agric_sec7_permanent_crops_by_crop_RWA_0.csv")
sec7tza<-read.csv("agric_sec7_permanent_crops_by_crop_TZA_0.csv")
sec7uga<-read.csv("agric_sec7_permanent_crops_by_crop_UGA_0.csv")
agr_sec7_permanent_crops_by_crop<-rbind(sec7gha,sec7rwa,sec7tza,sec7uga)
write.csv(agr_sec7_permanent_crops_by_crop, 
          file = "agr_sec7_permanent_crops_by_crop.csv")

# sec9
sec9gha<-read.csv("agric_sec9_byproducts_GHA_0.csv")
sec9rwa<-read.csv("agric_sec9_byproducts_RWA_0.csv")
sec9uga<-read.csv("agric_sec9_byproducts_UGA_0.csv")
agr_sec9_byproducts<-rbind(sec9gha,sec9rwa,sec9uga)
write.csv(agr_sec9_byproducts, file = "agr_sec9_byproducts.csv")

# sec10
sec10gha<-read.csv("agric_sec10_livestock_by_field_GHA_0.csv")
sec10rwa<-read.csv("agric_sec10_livestock_by_field_RWA_0.csv")
sec10tza<-read.csv("agric_sec10_livestock_by_field_TZA_0.csv")
sec10uga<-read.csv("agric_sec10_livestock_by_field_UGA_0.csv")
agr_sec10_livestock_by_field<-rbind(sec10gha,sec10rwa,sec10tza,sec10uga)
write.csv(agr_sec10_livestock_by_field, 
          file = "agr_sec10_livestock_by_field.csv")

# sec10a
sec10agha<-read.csv("agric_sec10a_livestock_GHA_0.csv")
sec10arwa<-read.csv("agric_sec10a_livestock_RWA_0.csv")
sec10atza<-read.csv("agric_sec10a_livestock_TZA_0.csv")
sec10auga<-read.csv("agric_sec10a_livestock_UGA_0.csv")
agr_sec10a_livestock<-rbind(sec10agha,sec10arwa,sec10atza,sec10auga)
write.csv(agr_sec10a_livestock, 
          file = "agr_sec10a_livestock.csv")

# sec10b
sec10bgha<-read.csv("agric_sec10b_livestock_products_GHA_0.csv")
sec10brwa<-read.csv("agric_sec10b_livestock_products_RWA_0.csv")
sec10btza<-read.csv("agric_sec10b_livestock_products_TZA_0.csv")
sec10buga<-read.csv("agric_sec10b_livestock_products_UGA_0.csv")
agr_sec10b_livestock_products<-rbind(sec10bgha,sec10brwa,sec10btza,sec10buga)
write.csv(agr_sec10b_livestock_products, 
          file = "agr_sec10b_livestock_products.csv")

# sec11
sec11gha<-read.csv("agric_sec11_implements_GHA_0.csv")
sec11rwa<-read.csv("agric_sec11_implements_RWA_0.csv")
sec11tza<-read.csv("agric_sec11_implements_TZA_0.csv")
sec11uga<-read.csv("agric_sec11_implements_UGA_0.csv")
agr_sec11_implements<-rbind(sec11gha,sec11rwa,sec11tza,sec11uga)
write.csv(agr_sec11_implements, 
          file = "agr_sec11_implements.csv")

# sec12
sec12gha<-read.csv("agric_sec12_extension_GHA_0.csv")
sec12rwa<-read.csv("agric_sec12_extension_RWA_0.csv")
sec12tza<-read.csv("agric_sec12_extension_TZA_0.csv")
sec12uga<-read.csv("agric_sec12_extension_UGA_0.csv")
agr_sec12_extension<-rbind(sec12gha,sec12rwa,sec12tza,sec12uga)
write.csv(agr_sec12_extension, 
          file = "agr_sec12_extension.csv")

# sec12a
sec12agha<-read.csv("agric_sec12a_extension_family_GHA_0.csv")
sec12arwa<-read.csv("agric_sec12a_extension_family_RWA_0.csv")
sec12atza<-read.csv("agric_sec12a_extension_family_TZA_0.csv")
sec12auga<-read.csv("agric_sec12a_extension_family_UGA_0.csv")
agr_sec12a_extension_family<-rbind(sec12agha,sec12arwa,sec12atza,sec12auga)
write.csv(agr_sec12a_extension_family, 
          file = "agr_sec12a_extension_family.csv")

# secA
secAgha<-read.csv("agric_secA_GHA_0.csv")
secArwa<-read.csv("agric_secA_RWA_0.csv")
secAtza<-read.csv("agric_secA_TZA_0.csv")
secAuga<-read.csv("agric_secA_UGA_0.csv")
agr_secA<-rbind(secAgha,secArwa,secAtza,secAuga)
write.csv(agr_secA, file = "agr_secA.csv")