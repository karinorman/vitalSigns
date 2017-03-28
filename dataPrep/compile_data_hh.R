# Nov. 3, 2016
# Compiling VS data

# set working directory for analysis code
setwd("~/Dropbox/vitalSigns/analysis/vital_signs")

# prep to combine raw data files
data.dir <- "../../data/Agricultural - Household/hh_data/raw files"
hh.data <- list.files(data.dir)

# combine secA
secA <- paste(data.dir, hh.data[grepl("secA", hh.data)],
                     sep="/")
secA.csv <- lapply(secA, read.csv)
secA.csv <- do.call(rbind, secA.csv)

# combine secB
secB <- paste(data.dir, hh.data[grepl("secB", hh.data)],
              sep="/")
secB.csv <- lapply(secB, read.csv)
secB.csv <- do.call(rbind, secB.csv)

# combine secC
secC <- paste(data.dir, hh.data[grepl("secC", hh.data)],
              sep="/")
secC.csv <- lapply(secC, read.csv)
secC.csv <- do.call(rbind, secC.csv)

# combine secE
secE <- paste(data.dir, hh.data[grepl("secE", hh.data)],
              sep="/")
secE.csv <- lapply(secE, read.csv)
secE.csv <- do.call(rbind, secE.csv)

# combine secHV1
secHV1 <- paste(data.dir, hh.data[grepl("secHV1", hh.data)],
              sep="/")
secHV1.csv <- lapply(secHV1, read.csv)
secHV1.csv <- do.call(rbind, secHV1.csv)

# combine secHV2
secHV2 <- paste(data.dir, hh.data[grepl("secHV2", hh.data)],
                sep="/")
secHV2.csv <- lapply(secHV2, read.csv)
secHV2.csv <- do.call(rbind, secHV2.csv)

# combine secI
secI <- paste(data.dir, hh.data[grepl("sec_I", hh.data)],
                sep="/")
secI.csv <- lapply(secI, read.csv)
secI.csv <- do.call(rbind, secI.csv)

# combine secJ1
secJ1 <- paste(data.dir, hh.data[grepl("secJ1", hh.data)],
              sep="/")
secJ1.csv <- lapply(secJ1, read.csv)
secJ1.csv <- do.call(rbind, secJ1.csv)

# combine secJ2
secJ2 <- paste(data.dir, hh.data[grepl("secJ2", hh.data)],
               sep="/")
secJ2.csv <- lapply(secJ2, read.csv)
secJ2.csv <- do.call(rbind, secJ2.csv)

# combine secK1
secK1 <- paste(data.dir, hh.data[grepl("secK1", hh.data)],
               sep="/")
secK1.csv <- lapply(secK1, read.csv)
secK1.csv <- do.call(rbind, secK1.csv)

# combine secK2
secK2 <- paste(data.dir, hh.data[grepl("secK2", hh.data)],
               sep="/")
secK2.csv <- lapply(secK2, read.csv)
secK2.csv <- do.call(rbind, secK2.csv)

# combine secL
secL <- paste(data.dir, hh.data[grepl("secL", hh.data)],
               sep="/")
secL.csv <- lapply(secL, read.csv)
secL.csv <- do.call(rbind, secL.csv)

# combine secN
secN <- paste(data.dir, hh.data[grepl("secN", hh.data)],
              sep="/")
secN.csv <- lapply(secN, read.csv)
secN.csv <- do.call(rbind, secN.csv)

# combine secU
secU <- paste(data.dir, hh.data[grepl("secU", hh.data)],
              sep="/")
secU.csv <- lapply(secU, read.csv)
secU.csv <- do.call(rbind, secU.csv)