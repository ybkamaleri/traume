####################
## laste opp data
###################

rm(list = ls())
library(data.table)

#########
## DATA
#########
filDir <- "~/avid/ntr/data/"
sr01 <- paste0(filDir, "datakilder1.R")
sr02 <- paste0(filDir, "datakilder2.R")
##ifelse(file.exists(sr01), source(sr01), source(sr02))
data <- ifelse(file.exists(sr01), sr01, sr02)
source(data)

###########################
## load skjemaer by index
###########################
indSkjema <- function(x, encode = "UTF-8"){
  indx <- grep(x, ntrCSV)
  datafile  <- paste0(filDir, ntrCSV[indx])
  DT <- fread(datafile, encoding = encode)
  return(DT)
}

traume <- indSkjema("Traume")
akutt <- indSkjema("Akutt")
intensiv <- indSkjema("Intensiv")
prehosp <- indSkjema("Prehospital")
ulykke <- indSkjema("Ulykke")
skade <- indSkjema("Skadegradering")
resh <- indSkjema("Resh", encode = "Latin-1")

###############################
## Resh HF
###############################
## endrer navn RHF

resh[RHF == "HSØ", RHF := "Helse Sør-Øst"]
resh[HF == "OUS", HF := "Oslo universitetssykehus"]

#################################
## Tar bort whitespace
#################################

delW <- function(dt){
  for (j in names(dt)) set(dt, j = j, value = dt[[trimws(j)]])
}

##ntrFil <- list(traume, akutt, intensiv, prehosp, ulykke, skade)

delW(traume)
delW(akutt)
delW(intensiv)
delW(prehosp)
delW(ulykke)
delW(skade)
delW(resh)

######################
## set key
setkey(traume, SkjemaGUID)
setkey(akutt, HovedskjemaGUID)
setkey(intensiv, HovedskjemaGUID)
setkey(prehosp, HovedskjemaGUID)
setkey(ulykke, HovedskjemaGUID)
setkey(skade, HovedskjemaGUID)


#########################################
## lager TraumeID liste med SkjemaGUID
masterID <- traume[, c("pt_id_ntr", "SkjemaGUID")]
names(masterID)[1] <- "ntrid" #rename pt_id_ntr to ntrid
setkey(masterID, SkjemaGUID)

#######################################################
## kobler ntrid til hvert skjema untatt Traume
#######################################################

## bruker "ntrid" istedenfor pt_id_ntr
akutt[masterID, ntrid := ntrid]
intensiv[masterID, ntrid := ntrid]
prehosp[masterID, ntrid := ntrid]
ulykke[masterID, ntrid := ntrid]
skade[masterID, ntrid := ntrid]

##########################################
## Endre variable navn ved bruk av indeks

## "Valgte ais" til "ais"
setnames(skade, grep("Valgte ais-koder", names(skade)), "ais")


#########################################
## Finner ut om eg. 900001.1 er skrevet som 090001.1 i data uttrekket
## fordi det bare vises som 900001.1 i GUI
