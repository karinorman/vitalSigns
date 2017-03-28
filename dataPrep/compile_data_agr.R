# Compile Agricultural Management Survey Data

# wd for Ag Management Intensity Survey
setwd("~/Dropbox/vitalSigns/analysis/vital_signs")

data.dir <- "../../data/Agricultural - Household/ag_data"

ag.data <- list.files(data.dir)

# Sec 1: Member roster
member.rost <- paste(data.dir, ag.data[grepl("member_roster", ag.data)],
                     sep = "/")
sec1.csv <- lapply(member.rost, read.csv)
sec1.csv <- do.call(rbind, sec1.csv)

# Sec 2: Field roster
field.rost <- paste(data.dir, ag.data[grepl("field_roster", ag.data)],
                   sep = "/")
sec2.csv <- lapply(field.rost, read.csv)
sec2.csv <- do.call(rbind, sec2.csv)

# Sec 3.7: Field details labor
field.det.lab <- paste(data.dir, ag.data[grepl("field_details_lab", ag.data)],
                    sep = "/")
sec3_7.csv <- lapply(field.det.lab, read.csv)
sec3_7.csv <- do.call(rbind, sec3_7.csv)

# Sec 3: Field details
field.det <- paste(data.dir, ag.data[grepl("sec3_field_details", ag.data)],
                   sep = "/")
sec3.csv <- lapply(field.det, read.csv)
sec3.csv <- do.call(rbind, sec3.csv)

# Sec 4: Crops by field
crops.by.field <- paste(data.dir, ag.data[grepl("sec4_crops_by_field", ag.data)],
                        sep = "/")
sec4.csv <- lapply(crops.by.field, read.csv)
sec4.csv <- do.call(rbind, sec4.csv)

# Sec 5: Crops by household
crops.by.hh <- paste(data.dir, ag.data[grepl("crops_by_hh", ag.data)],
                     sep = "/")
sec5.csv <- lapply(crops.by.hh, read.csv)
sec5.csv <- do.call(rbind, sec5.csv)

# Sec 6: Permanent crops by field
perm.crops.by.field <- paste(data.dir, ag.data[grepl("permanent_crops_by_field",
                                               ag.data)],
                             sep = "/")
sec6.csv <- lapply(perm.crops.by.field, read.csv)
sec6.csv <- do.call(rbind, sec6.csv)

# Sec 7: Permanent crops by crop
perm.crops.by.crop <- paste(data.dir, ag.data[grepl("permanent_crops_by_crop",
                                                    ag.data)],
                            sep = "/")
sec7.csv <- lapply(perm.crops.by.crop, read.csv)
sec7.csv <- do.call(rbind, sec7.csv)

# Sec 9: By-products
byproducts <- paste(data.dir, ag.data[grepl("byproducts", ag.data)],
                    sep = "/")
sec9.csv <- lapply(byproducts, read.csv)
sec9.csv <- do.call(rbind, sec9.csv)

# Sec 10: Livestock by field
livestock.by.field <- paste(data.dir, ag.data[grepl("livestock_by_field", 
                                                    ag.data)], sep = "/")
sec10.csv <- lapply(livestock.by.field, read.csv)
sec10.csv <- do.call(rbind, sec10.csv)

# Sec 10a: Livestock
livestock <- paste(data.dir, ag.data[grepl("10a_livestock", ag.data)],
                   sep = "/")
sec10a.csv <- lapply(livestock, read.csv)
sec10a.csv <- do.call(rbind, sec10a.csv)

# Sec 10b: Livestock Products
live.products <- paste(data.dir, ag.data[grepl("10b_livestock", ag.data)],
                       sep = "/")
sec10b.csv <- lapply(live.products, read.csv)
sec10b.csv <- do.call(rbind, sec10b.csv)
    
# Sec 11: Implements
implements <- paste(data.dir, ag.data[grepl("implements", ag.data)],
                    sep = "/")
sec11.csv <- lapply(implements, read.csv)
sec11.csv <- do.call(rbind, sec11.csv)

# Sec 12: Extension
extension <- paste(data.dir, ag.data[grepl("12_extension", ag.data)],
                   sep = "/")
sec12.csv <- lapply(extension, read.csv)
sec12.csv <- do.call(rbind, sec12.csv)

# Sec 12a: Extension Family
extension.family <- paste(data.dir, ag.data[grepl("extension_family", ag.data)],
                          sep = "/")
sec12a.csv <- lapply(extension.family, read.csv)
sec12a.csv <- do.call(rbind, sec12a.csv)

# Sec A
secA <- paste(data.dir, ag.data[grepl("secA_", ag.data)],
              sep = "/")
secA.csv <- lapply(secA, read.csv)
secA.csv <- do.call(rbind, secA.csv)


all.ag <- merge(sec1.csv, sec2.csv, all = TRUE)
all.ag <- merge(all.ag, sec3.csv, all = TRUE)

