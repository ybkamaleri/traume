## test ulykke module
#####################
library(data.table)
source("~/Git-work/traume/ntrApp/data.R")


setkey(masterFile, ntrid)
setkey(ulykke, ntrid)

regDT <- ulykke[masterFile]

datoFra <- as.Date("01-05-2017", format = "%d-%m-%Y")
datoTil <- as.Date("10-09-2018", format = "%d-%m-%Y")
alder <- 1:100
enhet <- "Finnmarkssykehuset"


## Liste over ulykke typer
navnUT <- c("acc_transport",
            "acc_fall",
            "acc_violence",
            "acc_self_inflict",
            "acc_work",
            "acc_sprt_recreat",
            "acc_fire_inhal",
            "acc_other")

## Legg alle acc typer
regDT[, alleUT := {v1 <- unlist(.SD) #ungroup .SDcols
  indUT <- which(v1 == 1)[1] #plukke index som oppfylle kravet
  list(v1[indUT], names(.SD)[indUT])}, #legge verdien på .SDcols ift. index indUT
  .SDcols = navnUT, by = 1:nrow(regDT)]


regData <- regDT[!duplicated(ntrid) & !is.na(ntrid)]


regData[, .N, by = HF]

## Ulykketyper
###############
## Alle
regData[alleUT == 1 &
          age %in% alder &
          dateAll %in% datoFra:datoTil,
        .N, by = HF] #Alle

## Fall
regData[acc_fall == 1 &
          age %in% alder &
          dateAll %in% datoFra:datoTil,
        .N, by = HF] #Fall

## Transport
regData[acc_transport == 1 &
          age %in% alder &
          dateAll %in% datoFra:datoTil,
        .N, by = HF] #Transport

## Arbeid
regData[acc_work == 1 &
          age %in% alder &
          dateAll %in% datoFra:datoTil,
        .N, by = HF] #Arbeid




## transport typer
regData[HF == enhet & age %in% alder & dateAll %in% datoFra:datoTil,
        .N, by = acc_trsp_rd_type]
