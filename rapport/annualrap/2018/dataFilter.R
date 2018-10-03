### Data Filter ###
###################

################
## TRAUME DATA
################

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
dataRawUl <- ulykke2[dateAll >= as.POSIXct(datoFra, format = "%Y-%m-%d") &
                      dateAll <= as.POSIXct(datoTil, format = "%Y-%m-%d")]


## bort med NA og duplicated - ntrid, alder og kjønn
dataUL <- dataRawUl[!is.na(ntrid) &
                      !duplicated(ntrid) &
                      !is.na(age) &
                      age != -1 &
                      !is.na(gender)]


## 2016 Data
dataRawUL16 <- ulykke2[dateAll >= as.POSIXct(datoFra01, format = "%Y-%m-%d") &
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
skade2[!is.na(inj_iss), iss15 := ifelse(inj_iss>15, 1, 0), by=seq_len(nrow(skade2))]
skade2[!is.na(inj_niss), niss15 := ifelse(inj_niss>15, 1, 0), by=seq_len(nrow(skade2))]

## 2017 Data
dataRawSK <- skade2[dateAll >= as.POSIXct(datoFra, format = "%Y-%m-%d") &
                     dateAll <= as.POSIXct(datoTil, format = "%Y-%m-%d")]


## bort med NA og duplicated - ntrid, alder og kjønn
dataSK <- dataRawSK[!is.na(ntrid) &
                      !duplicated(ntrid) &
                      !is.na(age) &
                      age != -1 &
                      !is.na(gender)]


## 2016 Data
dataRawSK16 <- skade2[dateAll >= as.POSIXct(datoFra01, format = "%Y-%m-%d") &
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


####################
## INTENSIVE DATA
####################

## 2017 data
dataRawIN <- intensiv2[dateSykehus >= as.POSIXct(datoFra, format = "%Y-%m-%d") &
                      dateSykehus <= as.POSIXct(datoTil, format = "%Y-%m-%d")]


## bort med NA og duplicated - ntrid, alder og kjønn
dataIN <- dataRawIN[!is.na(ntrid) &
                      !duplicated(ntrid) &
                      !is.na(age) &
                      age != -1 &
                      !is.na(gender)]


## 2016 Data
dataRawIN16 <- intensiv2[dateAll >= as.POSIXct(datoFra01, format = "%Y-%m-%d") &
                        dateAll <= as.POSIXct(datoTil01, format = "%Y-%m-%d")]


## bort med NA og duplicated - ntrid, alder og kjønn
dataIN16 <- dataRawIN16[!is.na(ntrid) &
                          !duplicated(ntrid) &
                          !is.na(age) &
                          age != -1 &
                          !is.na(gender)]

######################
## PREHOSPITAL DATA
######################

## 2017 data
dataRawPH <- prehosp2[dateSykehus >= as.POSIXct(datoFra, format = "%Y-%m-%d") &
                         dateSykehus <= as.POSIXct(datoTil, format = "%Y-%m-%d")]


## bort med NA og duplicated - ntrid, alder og kjønn
dataPH <- dataRawPH[!is.na(ntrid) &
                      !duplicated(ntrid) &
                      !is.na(age) &
                      age != -1 &
                      !is.na(gender)]


## 2016 Data
dataRawPH16 <- prehosp2[dateAll >= as.POSIXct(datoFra01, format = "%Y-%m-%d") &
                           dateAll <= as.POSIXct(datoTil01, format = "%Y-%m-%d")]


## bort med NA og duplicated - ntrid, alder og kjønn
dataPH16 <- dataRawPH16[!is.na(ntrid) &
                          !duplicated(ntrid) &
                          !is.na(age) &
                          age != -1 &
                          !is.na(gender)]


