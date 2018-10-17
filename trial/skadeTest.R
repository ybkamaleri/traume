
source("~/Git-work/traume/ntrApp/data.R")

## OBS!! bruk 'aisMix' for å velge skade gradering
dataRaw <- skade #her skal det merge med dataFiltert
dataRaw[, aisMix := toString(unlist(strsplit(ais, split = ","))), by = ntrid]


dataIN <- dataRaw[!duplicated(ntrid)]
dataIN[, ais := NULL] #slett ais siden aisMix inneholder alle ais koder


kode_skjelett <- "^6502[1-3][024678].*[23]$"
kode_rygg <- "^6402.*[3-6]$"

## Lager subset data
## kode er skjelettskader og kode1 ryggmargsskade
dataSK <- dataIN[, list(
  kode = ifelse(
    sum(grepl(kode_skjelett,
      trimws(unlist(strsplit(aisMix, split = ",")))), na.rm = TRUE) != 0, 1, 0),
  kode1 = ifelse(
    sum(grepl(kode_rygg,
      trimws(unlist(strsplit(aisMix, split = ",")))), na.rm = TRUE) != 0, 1, 0),
  ntrid = ntrid,
  gender = gender,
  age = age), by = ntrid]

## kode1 == 1 hvis begge skjelettskader og ryggmargsskade
dataSK[, kode2 := ifelse(kode == 1 & kode1 == 1, 1, 0), by = ntrid]

if (as.numeric(input$til_cerv) == 1){
  data <- tilSpine()
} else if (as.numeric(input$til_cerv) == 2){
  ## bare de med skjelettskader uten rygmargsskade
  data <- dataSK[kode == 1 & kode2 == 0,
    list(n = 1, gender = gender, age = age), by = ntrid]
} else {
  data <- dataSK[kode1 == 1, list(n = 1, gender = gender, age = age), by = ntrid]
}
