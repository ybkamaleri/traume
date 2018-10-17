## test AIS module
#####################
library(data.table)
source("~/Git-work/traume/ntrApp/data.R")


setkey(masterFile, ntrid)
setkey(skade, ntrid)

regDT <- skade[masterFile]

datoFra <- as.Date("15-09-2017", format = "%d-%m-%Y")
datoTil <- as.Date("10-09-2018", format = "%d-%m-%Y")
alder <- 0:110
enhet <- "Finnmarkssykehuset"

## RENSE
dataRaw <- copy(regDT)
dataRaw[, aisMix := toString(unlist(strsplit(ais, split = ","))), by = ntrid]

dataIN <- dataRaw[!duplicated(ntrid)]
dataIN[, ais := NULL] #slett ais siden aisMix inneholder alle ais koder


## Spine - Lumbalcolumna
kode_skjelett <- "^6506[123][024678].*[23]$"
kode_rygg <- "^6406.*[345]$"

## kode er skjelettskader og kode2 ryggmargsskade
dataSK <- dataIN[, list(
  kode = ifelse(
    sum(grepl(kode_skjelett,
              unlist(strsplit(aisMix, split = ",")))) != 0, 1, 0),
  kode2 = ifelse(
    sum(grepl(kode_rygg,
              unlist(strsplit(aisMix, split = ",")))) != 0, 1, 0),
  ntrid = ntrid,
  gender = gender), by = ntrid]

#kode1 0 hvis begge skjelettskader og ryggmargsskade
dataSK[, kode1 := kode, by = ntrid]
dataSK[, kode1 := ifelse(kode == 1 && kode2 == 1, 0, kode), by = ntrid]

dataSK

dataSK[kode1 == 1, list(n = 1, gender = gender), by = ntrid]
dataSK[kode2 == 1, list(n = 1, gender = gender), by = ntrid]
