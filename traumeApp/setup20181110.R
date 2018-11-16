## Dato with as.POSIXc

## laste opp data
###################
rm(list = ls())

## DATA
#########
filDir <- "~/avid/ntr/data/"

## sr01 <- paste0(filDir, "datakilder1.R")
## sr02 <- paste0(filDir, "datakilder2.R")
## ##ifelse(file.exists(sr01), source(sr01), source(sr02))
## data <- ifelse(file.exists(sr01), sr01, sr02)

kilde <- "~/avid/ntr/data/datakilder1.R"

source(kilde)

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

## Merge begge filer ved å beholder alle i baseFile. Row i sykehusFile som ikke har
## kobling til SkjemaGUID i traumeskjema blir slettet ved lagring av masterFile
masterFile <- sykehusFile[baseFile, on = c(HovedskjemaGUID = "SkjemaGUID")]

## set key for masterFile (base og sykehus data)
setkeyv(masterFile, c("HovedskjemaGUID", "UnitId"))

## tar bort prefix "NTR-" for ntr-ID
masterFile[, ntrid := as.numeric(gsub("^NTR-", "", pt_id_ntr))]


##########################################################################
## OBS!!! -- Dette skal slettes når man kan trekke ut de fra MRS direkte
## legge RHF, HF og Sykehusnavn fra resh fil
resh <- indSkjema("Resh", encode = "Latin-1")
delW(resh)
setkey(resh, reshid)
reshVars <- names(resh)[2:4]
masterFile[resh, on = c(UnitId = "reshid"), (reshVars) := mget(reshVars)]


##########################################################################



## Endre kolonenavn og var list for merge til andre filer
##########################################################
changeName <- c("age",
                "gender",
                "dateSykehus",
                "dateAll",
                "hospSerial")

setnames(masterFile, c("PatientAge",
                       "PatientGender",
                       "ed_arrival_dtg",
                       "inj_start_date",
                       "hosp_serial_num"), changeName)

## Dato format
##############################
## timeSykehus - dato med klokkelsett
## dateAll og dateSykehus inneholder bare dato
masterFile[, `:=` (timeSykehus = as.POSIXct(dateSykehus, format = "%d.%m.%Y %H:%M:%S"),
  dateAll = as.POSIXct(dateAll, format = "%d.%m.%Y %H:%M:%S"),
  dateSykehus = as.POSIXct(dateSykehus, format = "%d.%m.%Y %H:%M:%S"))]



## merge selected variables from masterFile to all files
##################################
## newName <- c("pt_id_ntr","ntrid","gender","age","dateAll","dateSykehus", "timeSykehus", "hospSerial")
newName <- names(masterFile)[2:ncol(masterFile)]
akutt[masterFile, on = .(HovedskjemaGUID), (newName) := mget(newName)]
intensiv[masterFile, on = .(HovedskjemaGUID), (newName) := mget(newName)]
prehosp[masterFile, on = .(HovedskjemaGUID), (newName) := mget(newName)]
ulykke[masterFile, on = .(HovedskjemaGUID), (newName) := mget(newName)]
skade[masterFile, on = .(HovedskjemaGUID), (newName) := mget(newName)]
