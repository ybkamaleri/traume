source("../ntrApp/data.R")

masterFile[duplicated(ntrid),.N] #antall duplicated ntrid
masterFile[!duplicated(ntrid), .N] #antall unique
dim(masterFile)

wday <- masterFile[!is.na(dateSykehus), list(wday = weekdays(dateSykehus),
                                             ntrid = ntrid,
                                             date = dateSykehus)]

wdayN <- masterFile[!is.na(dateSykehus), .N, by = dateSykehus]
wdayN[, .(sum = sum(N))]

## Eksluderer duplicated ntrid
wdayNA <- masterFile[!duplicated(ntrid) & !is.na(dateSykehus), .N, by = dateSykehus]
wdayNA[, .(sum = sum(N))]

## Akutt skjema
akkdayN <- akutt[!is.na(dateSykehus), .N, by = dateSykehus]
akkdayN[, .(sum = sum(N))]

## Eksluderer duplicated ntrid
akkdayNA <- akutt[!duplicated(ntrid) & !is.na(dateSykehus), .N, by = dateSykehus]
akkdayNA[, .(sum = sum(N))]

##### Bruk denne metoden ######
## ukedager slik skal gjøres:
## - valg tidsrom
## - lager new col med dager
## - teller dagene

valgDato <- akutt[!duplicated(ntrid) & !is.na(dateSykehus) &
                    dateSykehus >= as.Date("2015-01-01", format = "%Y-%m-%d") &
                     dateSykehus <= as.Date("2018-01-01", format = "%Y-%m-%d")]
valgDag <- valgDato[, dag := weekdays(dateSykehus)]
ntot <- dim(valgDag)[1] #total dager
ukeDag <- valgDag[, .(pros = round((.N / ntot) * 100),
                      n = .N), by = dag]

library(ggplot2)
ukeDag$dag <- factor(ukeDag$dag, levels = c("mandag", "tirsdag", "onsdag", "torsdag",
                                            "fredag", "lørdag", "søndag"))

ggplot(ukeDag, aes(dag, pros)) + geom_bar(stat = "identity")
