## Årsrapport 2018

rm(list = ls())

pkg <- c("data.table", "rreg", "ggplot2")

sapply(pkg, require, character.only = TRUE)

## bytt NA med 0
bNA <- function(DT, na = 0){
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j, na)
}


## hent data
source("/home/yuskam/Git-work/traume/ntrApp/data.R")

## hent ekstert funksjoner
source("/home/yuskam/Git-work/traume/ntrApp/misc/byttna.R")


## Valg muligheter
datoFra <- "2017-01-01"
datoTil <- "2017-12-31"

datoFra01 <- "2016-01-01"
datoTil01 <- "2016-12-31"


## Prepare data
################################

## Antall traume 2017
dataRaw <- masterFile[dateAll >= as.Date(datoFra, format = "%Y-%m-%d") &
                        dateAll <= as.Date(datoTil, format = "%Y-%m-%d")]


## bort med NA og duplicated - ntrid, alder og kjønn
data <- dataRaw[!is.na(ntrid) &
                  !duplicated(ntrid) &
                   !is.na(age) &
                   age != -1 &
                   !is.na(gender)]


## Antall traume 2016
dataRaw16 <- masterFile[dateAll >= as.Date(datoFra01, format = "%Y-%m-%d") &
                          dateAll <= as.Date(datoTil01, format = "%Y-%m-%d")]


## bort med NA og duplicated - ntrid, alder og kjønn
data16 <- dataRaw16[!is.na(ntrid) &
                      !duplicated(ntrid) &
                   !is.na(age) &
                   age != -1 &
                   !is.na(gender)]

##################
## Antall traume
##################
## regional deling
ageMan <- data[gender == 1, list(mann = .N), by = list(RHF, age)]
ageKvinne <- data[gender == 2, list(kvinne = .N), by = list(RHF, age)]
ageMK <- merge(ageMan, ageKvinne, all = TRUE)


## bytt NA med 0
bNA(ageMK)

## Lager total
ageMK[, alle := mann + kvinne, by = list(RHF, age)]

## Gir nytt navn
newNavn <- c("RHF", "Alder", "Menn", "Kvinner", "Alle")
data.table::setnames(ageMK, 1:5, newNavn)
ageMK

## konverterer data til long
dataLong <-melt(ageMK, id.vars=c("RHF", "Alder"),
                measure.vars=c("Menn","Kvinner","Alle"),
                  variable.name="gender", value.name="n")

(antallTraume <- ggplot(dataLong) +
   geom_line(aes(x = Alder, y = n, group = gender, color = gender)) + facet_wrap( ~ RHF))


## 2017 vs 2016
age17 <- data[, list(age17 = .N), by = age]
age16 <- data16[, list(age16 = .N), by = age]

ageMix <- merge(age17, age16, all = TRUE)
bNA(ageMix)

ageLong <- melt(ageMix, id.vars = "age",
                measure.vars = c("age16", "age17"),
                variable.name = "year",
                value.name = "n")

(antall1617 <- ggplot(ageLong, aes(age, n)) +
   geom_line(aes(group = year, color = year), stat = "identity"))
