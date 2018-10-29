
## Dato with as.POSIXct

## laste opp data
###################
rm(list = ls())
library(data.table)

## DATA
#########
filDir <- "~/avid/ntr/data/"
sr01 <- paste0(filDir, "datakilder1.R")
sr02 <- paste0(filDir, "datakilder2.R")
##ifelse(file.exists(sr01), source(sr01), source(sr02))
data <- ifelse(file.exists(sr01), sr01, sr02)
source(data)

## lager index
###########################
indSkjema <- function(x, encode = "UTF-8"){
  indx <- grep(x, ntrCSV)
  datafile  <- paste0(filDir, ntrCSV[indx])
  DT <- fread(datafile, encoding = encode)
  return(DT)
}

## load skjemaer by index
###########################
traume <- indSkjema("Traume")
akutt <- indSkjema("Akutt")
intensiv <- indSkjema("Intensiv")
prehosp <- indSkjema("Prehospital")
ulykke <- indSkjema("Ulykke")
skade <- indSkjema("Skadegradering")


## Resh HF
###############################
## endrer navn RHF
## resh[RHF == "HSØ", RHF := "Helse Sør-Øst"]
## resh[HF == "OUS", HF := "Oslo universitetssykehus"]


## Tar bort whitespace
#################################
delW <- function(dt){
  for (j in names(dt)) set(dt, j = j, value = dt[[trimws(j)]])
}
#ntrFil <- list(traume, akutt, intensiv, prehosp, ulykke, skade)
delW(traume)
delW(akutt)
delW(intensiv)
delW(prehosp)
delW(ulykke)
delW(skade)

## set key
###########
setkey(traume, SkjemaGUID)
setkey(akutt, HovedskjemaGUID)
setkey(intensiv, HovedskjemaGUID)
setkey(prehosp, HovedskjemaGUID)
setkey(ulykke, HovedskjemaGUID)
setkey(skade, HovedskjemaGUID)

## "Valgte ais" til "ais"
###########################################
setnames(skade, grep("Valgte ais-koder", names(skade)), "ais")

## Master File
################
## slett row med missing ntrid eller pt_id_ntr == ""

baseFile <- traume[pt_id_ntr != "", c("pt_id_ntr",
                                      "UnitId",
                                      "hosp_serial_num",
                                      "SkjemaGUID",
                                      "PatientAge",
                                      "PatientGender",
                                      "inj_start_date")]
## henter dato for datovalg til Virksomhetrapport
sykehusFile <- akutt[, c("HovedskjemaGUID", "ed_arrival_dtg")]

## Merge begge filer ved å beholder alle i baseFile. Row i akuttfil som ikke har
## kombling til SkjemaGUID i traume blir slettes ved koblingen til bsFile
bsFile <- sykehusFile[baseFile, on = c(HovedskjemaGUID = "SkjemaGUID")]

## set key for bsFile (base og sykehus data)
setkeyv(bsFile, c("HovedskjemaGUID", "UnitId"))

## tar bort prefix "NTR-" for ntr-ID
bsFile[, ntrid := as.numeric(gsub("^NTR-", "", pt_id_ntr))]


##########################################################################
## OBS!!! -- Dette skal slettes når man kan trekke ut de fra MRS direkte
## legge RHF, HF og Sykehusnavn fra resh fil
resh <- indSkjema("Resh", encode = "Latin-1")
delW(resh)
setkey(resh, reshid)
masterFile <- bsFile[resh, on = c(UnitId = "reshid")]


##########################################################################



## Endre kolonenavn og var list for merge til andre filer
##########################################################
changeName <- c("age",
                "gender",
                "dateSykehus",
                "dateAll")

setnames(masterFile, c("PatientAge",
                       "PatientGender",
                       "ed_arrival_dtg",
                       "inj_start_date"), changeName)

## Dato format
##############################
## timeSykehus - dato med klokkelsett
## dateAll og dateSykehus inneholder bare dato
masterFile[, `:=` (timeSykehus = as.POSIXct(dateAll, format = "%d.%m.%Y %H:%M:%S"),
  dateAll = as.POSIXct(dateAll, format = "%d.%m.%Y %H:%M:%S"),
  dateSykehus = as.POSIXct(dateAll, format = "%d.%m.%Y %H:%M:%S"))]



## merge selected variables from masterFile to all files
##################################
newName <- c("pt_id_ntr","ntrid","gender","age","dateAll","dateSykehus", "timeSykehus", "hosp_serial_num")
akutt[masterFile, on = .(HovedskjemaGUID), (newName) := mget(paste0("i.", newName))]
intensiv[masterFile, on = .(HovedskjemaGUID), (newName) := mget(paste0("i.", newName))]
prehosp[masterFile, on = .(HovedskjemaGUID), (newName) := mget(paste0("i.", newName))]
ulykke[masterFile, on = .(HovedskjemaGUID), (newName) := mget(paste0("i.", newName))]
skade[masterFile, on = .(HovedskjemaGUID), (newName) := mget(paste0("i.", newName))]


## setkey
#####################
setkey(masterFile, ntrid)

## akutt med HF, RHF, Hospital
##############################
akutt2 <- akutt[resh, on = c(UnitId = "reshid")]
