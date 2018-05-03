rm(list = ls())
library(data.table)

#########
## DATA
#########
filDir <- getwd()
sr01 <- paste0(filDir,"/", "datakilder1.R")
sr02 <- paste0(filDir, "/", "datakilder2.R")
##ifelse(file.exists(sr01), source(sr01), source(sr02))
data <- ifelse(file.exists(sr01), sr01, sr02)
source(data)

###########################
## load skjemaer by index
###########################
indSkjema <- function(x, encode = "UTF-8"){
  indx <- grep(x, ntrCSV)
  DT <- fread(ntrCSV[indx], encoding = encode)
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
## X[Y] - beholder all Y
#######################################################
## akutt <- ntrid[akutt]
## intensiv <- ntrid[intensiv]
## prehosp <- ntrid[prehosp]
## ulykke <- ntrid[ulykke]
## skade <- ntrid[skade]

## bruker "ntrid" istedenfor pt_id_ntr
akutt[masterID, ntrid := ntrid]
intensiv[masterID, ntrid := ntrid]
prehosp[masterID, ntrid := ntrid]
ulykke[masterID, ntrid := ntrid]
skade[masterID, ntrid := ntrid]
##########################################
## Endre variable navn ved bruk av indeks

## Valgte ais-koder
indAis <- grep("Valgte ais", names(skade)) #finne indeks til kolonne
names(skade)[indAis] <- "ais"  #gir nytt navn til Valgte ais-koder

################################################
## kombinere alle skadekoder fra samme NTR-nr
## og tar bort dublikate koder
###############################################
#ta bort alle missing NTR-nr.
skade <- skade[!is.na(ntrid), ]

## kombinere alle skadekoder og valgt bare unike koder
## fra forskjellige sykehus for hver NTR-nr og variable navn blir "aiskode"
skade[skade[!is.na(ntrid),
            toString(unique(unlist(
              strsplit(ais, split = ",")))), by = ntrid],
      on = "ntrid", aiskode := i.V1]


##########################
## Dele skade koder til flere kolonne
## install.packages("splitstackshape", repos = "https://cran.uib.no", dependencies = TRUE)
library(splitstackshape)
skadeDel <- splitstackshape::cSplit(skade, splitCols = "aiskode", sep = ",", direction = "wide", drop = FALSE)


################################
## Velge ulykke og skadegrader

##################################
## merge ulykke og skade skjemaer
##################################
## ## beholder relevant kolonne fra ulykkeskjema
## accName <- grep("acc_transport", names(ulykke)):grep("acc_fire_inhal", names(ulykke))
## ulykkeSub <- ulykke[, .SD, .SDcols = accName] #valg bare disse kolonner

### Beholder alle var i skadeskjema
### alle var starter med i. kommer fra skade skjema
skadeGrad <- ulykke[skade, on = "ntrid"]


## henter index fra acc_trans til acc_fire
accName <- grep("acc_transport", names(skadeGrad)):grep("acc_fire_inhal", names(skadeGrad))
## convert to numeric
for (d in accName) {
  set(skadeGrad, j = d, value = as.numeric(skadeGrad[[d]]))
}

#######################
## Valg ulyketype
#######################
acd <- 1
body <- 4
grad1 <- 1
grad2 <- 1
varValg <- "acc_trsp_rd_type"

setkey(skadeGrad, ntrid)

##testGrad <- skadeGrad[!duplicated(ntrid) & !is.na(ntrid), ]

skadeAlvor <- skadeGrad[get(varValg) == acd & !duplicated(ntrid) & !is.na(ntrid), list(ja = ifelse(sum(grepl(
  paste0("^", body, ".*[", grad1, "-", grad2, "]$"), as.numeric(unlist(
    strsplit(aiskode, split = ","))))) != 0, 1, 0),
  syk = i.HealthUnitName), #bruk i.HealthUnitName som kommer fra skadeskjema
  by = c("ntrid")]

skadeAlvor[ja == 1, .N, by = syk]


#########################
## Andre ulykke
########################
## 1 - Fallulykke
## 2 - Voldsulykke
## 3 - Arbeidsulykke
## 4 - Sport og fritid
## 5 - Brann og inhalasjonsskade
## 6 - Annen ulykke

accT <- 4
body <- 4
grad1 <- 1
grad2 <- 1

accKode <- switch(accT,
                  "acc_fall",
                  "acc_violence",
                  "acc_self_inflict",
                  "acc_work",
                  "acc_sprt_recreat",
                  "acc_fire_inhal",
                  "acc_other")

setkey(skadeGrad, ntrid)

##testGrad <- skadeGrad[!duplicated(ntrid) & !is.na(ntrid), ]

skadeAndre <- skadeGrad[get(accKode) == 1 & !duplicated(ntrid) & !is.na(ntrid), list(ja = ifelse(sum(grepl(
  paste0("^", body, ".*[", grad1, "-", grad2, "]$"), as.numeric(unlist(
    strsplit(aiskode, split = ","))))) != 0, 1, 0),
  syk = i.HealthUnitName), #bruk i.HealthUnitName som kommer fra skadeskjema
  by = c("ntrid")]

skadeAndre[ja == 1, .N, by = syk]
