## laste opp data
###################
rm(list = ls())

################
## sti til data
################
source("K:/Sensitivt/kvalitetsregistre/2013-9541_Nasjonalt_traumeregister/Yusman/data/misc/datakilder.R")
ntrDir <- "K:/Sensitivt/kvalitetsregistre/2013-9541_Nasjonalt_traumeregister/Yusman/data/" #produserer 'ntrCSV' og 'miscCSV'
miscDir <- "K:/Sensitivt/kvalitetsregistre/2013-9541_Nasjonalt_traumeregister/Yusman/data/misc/"

###########################
## load skjemaer by index
###########################
indSkjema <- function(x, ntrdir = TRUE, fname = ntrCSV, encode = "Latin-1"){
  indx <- grep(x, fname)
  
  if (ntrdir) {
    datafile  <- paste0(ntrDir, fname[indx])
  } else {
    datafile  <- paste0(miscDir, fname[indx])
  }
  DT <- data.table::fread(datafile, encoding = encode)
  return(DT)
}


traumeRaw <- indSkjema("Traume")
akutt <- indSkjema("Akutt")
intensiv <- indSkjema("Intensiv")
prehosp <- indSkjema("Prehospital")
ulykke <- indSkjema("Ulykke")
skade <- indSkjema("Skadegradering")
## bruk miscDir for å lese filen
resh <- indSkjema("Resh", ntrdir = FALSE, fname = miscCSV, encode = "Latin-1")


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
delW(traumeRaw)
delW(akutt)
delW(intensiv)
delW(prehosp)
delW(ulykke)
delW(skade)

## set key
###########
setkey(traumeRaw, SkjemaGUID)
setkey(akutt, HovedskjemaGUID)
setkey(intensiv, HovedskjemaGUID)
setkey(prehosp, HovedskjemaGUID)
setkey(ulykke, HovedskjemaGUID)
setkey(skade, HovedskjemaGUID)

## tar bort alle som ikke har riktig trume nr dvs. starter med NTR og uten dato på inj_start_date
traumeRaw[, bortID := nchar(pt_id_ntr), by=seq_len(nrow(traumeRaw))][, bort := nchar(inj_start_date), by=seq_len(nrow(traumeRaw))]
traume <- traumeRaw[bort==16, ] #slett uten riktig dato
traume <- traume[bortID==10, ] #slett uten riktig NTR-nr

# Omkode ting og tang
gender <- c("Male", "Female") #1-male 2-Female
genderkode <- 1:2
traume[, kjonn := as.integer(ifelse(Patient_Gender=="Male", 1, ifelse(Patient_Gender=="Female", 2, NA)))]
# traume[list(Patient_Gender=gender, to=genderkode), on="Patient_Gender", Patient_Gender := i.to]

## "Valgte ais" til "ais"
###########################################
setnames(skade, grep("Valgte ais-koder", names(skade)), "ais")

## Master File
################
baseFile <- traume[, c("pt_id_ntr",
                       "UnitId",
                       "hosp_serial_num",
                       "SkjemaGUID",
                       "Patient_Age",
                       "kjonn",
                       "HealthUnitName",
                       "inj_start_date")]
sykehusFile <- akutt[, c("HovedskjemaGUID", "ed_arrival_dtg")]

## Beholder alle i baseFile. Row i akuttfil som ikke har kombling til SkjemaGUID i
## traume slettes
bsFile <- sykehusFile[baseFile, on = c(HovedskjemaGUID = "SkjemaGUID")]

## set key for bsFile (base og sykehus data)
setkeyv(bsFile, c("HovedskjemaGUID", "UnitId"))

## tar bort prefix "NTR-" for ntr-ID
bsFile[, ntrid := as.numeric(gsub("^NTR-", "", pt_id_ntr))]

##########################################################################
## OBS!!! -- Dette skal slettes når man kan trekke ut de fra MRS direkte
## legge RHF, HF og Sykehusnavn fra resh fil
# resh <- indSkjema("Resh", encode = "Latin-1")
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

setnames(masterFile, c("Patient_Age",
                       "kjonn",
                       "ed_arrival_dtg",
                       "inj_start_date"), changeName)

# konverter sting til tall
masterFile[, age := as.numeric(age)]

## Dato format
##############################
## timeSykehus - dato med klokkelsett
## dateAll og dateSykehus inneholder bare dato
masterFile[, `:=` (timeSykehus = as.POSIXct(dateAll, format = "%d.%m.%Y %H:%M"),
                   dateAll = as.POSIXct(dateAll, format = "%d.%m.%Y %H:%M"),
                   dateSykehus = as.POSIXct(dateAll, format = "%d.%m.%Y %H:%M"))]



## merge selected variables from masterFile to all files
##################################
newName <- c("pt_id_ntr","ntrid","gender","age","dateAll","dateSykehus", "timeSykehus")
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

