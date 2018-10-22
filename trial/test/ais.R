## Test for AIS koder

library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)
library(shinyBS)

source("~/Git-work/traume/ntrApp/data2.R")
source("~/Git-work/traume/traumeApp/functions/byttNA.R")

## Filter variabler
## ================

## Liste av variablenavn i ullyke datasett som skal plukkes ut
valgCol <- c("acc_transport",
  "acc_fall",
  "acc_violence",
  "acc_self_inflict", #selvpåført
  "acc_work",
  "acc_sprt_recreat", #sport og fritid
  "acc_fire_inhal",
  "acc_other",
  "acc_trsp_rd_type", #transport typer
  "ntrid")

## Liste over ulykke typer
navnUT <- c("acc_transport",
  "acc_fall",
  "acc_violence",
  "acc_self_inflict",
  "acc_work",
  "acc_sprt_recreat",
  "acc_fire_inhal",
  "acc_other")

regDT = ulykke[!duplicated(ntrid) & !is.na(ntrid), valgCol, with = FALSE]

## Legg alle type ulykke - alleUT : alle ulykke typer
#######################################################
regDT[, alleUT := {
  v1 <- unlist(.SD) #ungroup .SDcols
  indUT <- which(v1 == 1)[1] #plukke index som oppfylle kravet
  list(v1[indUT], names(.SD)[indUT])}, #legge verdien på .SDcols ift. index indUT
  .SDcols = navnUT, by = 1:nrow(regDT)]

regDT[, .N, by = alleUT]

## Missing alle ullyketyper
## ========================
## bytt NA med 0

for (j in navnUT) {set(regDT, which(regDT[[j]] != 1), j, 10)}
regDT[, Nsum := rowSums(.SD, na.rm = T), .SDcols = navnUT]
regDT[, .N, by = Nsum] #80 means alle er NA


## Skade data
## ===========
dataRaw <- skade[regDT, on = c(ntrid = "ntrid")]
dataRaw[, aisMix := toString(unlist(strsplit(ais, split = ","))), by = ntrid]

dataRaw[ntrid == 7058, c("ntrid", "ais", "aisMix")]
