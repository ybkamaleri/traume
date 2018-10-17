## Valg data basert på datoFra og datoTil objects

####################
## TRAUME DATA
####################
## Antall traume 2017
dataRaw <- masterFile[dateAll >= as.POSIXct(datoFra, format = "%Y-%m-%d") &
                        dateAll <= as.POSIXct(datoTil, format = "%Y-%m-%d"),]


## bort med NA og duplicated - ntrid, alder og kjønn
data <- dataRaw[!is.na(ntrid) &
                  !duplicated(ntrid) &
                   !is.na(age) &
                   age != -1 &
                   !is.na(gender)]


## Antall traume 2016
dataRaw16 <- masterFile[dateAll >= as.POSIXct(datoFra01, format = "%Y-%m-%d") &
                          dateAll <= as.POSIXct(datoTil01, format = "%Y-%m-%d"),]


## bort med NA og duplicated - ntrid, alder og kjønn
data16 <- dataRaw16[!is.na(ntrid) &
                      !duplicated(ntrid) &
                       !is.na(age) &
                       age != -1 &
                       !is.na(gender)]

###################
## ULYKKE DATA
###################

## 2017 Data
dataRawUl <- ulykke[dateAll >= as.POSIXct(datoFra, format = "%Y-%m-%d") &
                      dateAll <= as.POSIXct(datoTil, format = "%Y-%m-%d")]


## bort med NA og duplicated - ntrid, alder og kjønn
dataUL <- dataRawUl[!is.na(ntrid) &
                      !duplicated(ntrid) &
                       !is.na(age) &
                       age != -1 &
                       !is.na(gender)]


## 2016 Data
dataRawUL16 <- ulykke[dateAll >= as.POSIXct(datoFra01, format = "%Y-%m-%d") &
                        dateAll <= as.POSIXct(datoTil01, format = "%Y-%m-%d")]


## bort med NA og duplicated - ntrid, alder og kjønn
dataUL16 <- dataRawUL16[!is.na(ntrid) &
                          !duplicated(ntrid) &
                           !is.na(age) &
                           age != -1 &
                           !is.na(gender)]

#########################
## SKADE DATA
#########################
# kode om iss > 15
skade[, iss15 := ifelse(inj_iss>15, 1, 0), by=seq_len(nrow(skade))]

## 2017 Data
dataRawSK <- skade[dateAll >= as.POSIXct(datoFra, format = "%Y-%m-%d") &
                     dateAll <= as.POSIXct(datoTil, format = "%Y-%m-%d")]


## bort med NA og duplicated - ntrid, alder og kjønn
dataSK <- dataRawSK[!is.na(ntrid) &
                      !duplicated(ntrid) &
                       !is.na(age) &
                       age != -1 &
                       !is.na(gender)]


## 2016 Data
dataRawSK16 <- skade[dateAll >= as.POSIXct(datoFra01, format = "%Y-%m-%d") &
                       dateAll <= as.POSIXct(datoTil01, format = "%Y-%m-%d")]


## bort med NA og duplicated - ntrid, alder og kjønn
dataSK16 <- dataRawSK16[!is.na(ntrid) &
                          !duplicated(ntrid) &
                           !is.na(age) &
                           age != -1 &
                           !is.na(gender)]


#####################
## AKUTT DATA
#####################

## 2017 data
dataRawAK <- akutt2[dateSykehus >= as.POSIXct(datoFra, format = "%Y-%m-%d") &
                      dateSykehus <= as.POSIXct(datoTil, format = "%Y-%m-%d")]


## bort med NA og duplicated - ntrid, alder og kjønn
dataAK <- dataRawAK[!is.na(ntrid) &
                      !duplicated(ntrid) &
                       !is.na(age) &
                       age != -1 &
                       !is.na(gender)]


## 2016 Data
dataRawAK16 <- akutt2[dateAll >= as.POSIXct(datoFra01, format = "%Y-%m-%d") &
                        dateAll <= as.POSIXct(datoTil01, format = "%Y-%m-%d")]


## bort med NA og duplicated - ntrid, alder og kjønn
dataAK16 <- dataRawAK16[!is.na(ntrid) &
                          !duplicated(ntrid) &
                           !is.na(age) &
                           age != -1 &
                           !is.na(gender)]
