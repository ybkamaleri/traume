## Årsrapport 2018

rm(list = ls())

pkg <- c("data.table", "rreg", "ggplot2", "directlabels")

sapply(pkg, require, character.only = TRUE)

## bytt NA med 0
bNA <- function(DT, na = 0){
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j, na)
}


## hent data
source("~/Git-work/traume/ntrApp/data.R")

## hent ekstert funksjoner
source("~/Git-work/traume/ntrApp/misc/byttna.R")


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
newNavn <- c("RHF", "Alder", "Menn", "Kvinner", "Begge")
data.table::setnames(ageMK, 1:5, newNavn)
ageMK

## konverterer data til long
dataLong <-melt(ageMK, id.vars=c("RHF", "Alder"),
                measure.vars=c("Menn","Kvinner","Begge"),
                variable.name="gender", value.name="n")

cols <- c("#4292c6", "#c6dbef", "#084594")
cols2 <- c("#FF7260", "#2171b5")

pthemes <- theme(axis.text.y = element_text(size = 10, color = "black"),
                 axis.text.x = element_text(size = 10, color = "black"),
                 axis.ticks.y = element_blank(),
                 axis.line.x = element_line(size = 0.5),
                 axis.title.y = element_text(size = 11),
                 axis.title.x = element_text(size = 11),
                 panel.background = element_rect(fill = "white"),
                 panel.border = element_rect(linetype = 1, fill = NA, color = "white"),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.major.y = element_line(linetype = 2, color = "grey"),
                 legend.position = "bottom",
                 legend.title = element_blank(),
                 legend.text = element_text(size = 11),
                 legend.key = element_rect(fill = "white")
                 )

maxRN <- max(dataLong$n)
(antallTraume <- ggplot(dataLong) +
   geom_line(aes(x = Alder, y = n, group = gender, color = gender), size = 0.7) +
   ## scale_linetype_manual(values = c(1,1,4)) +
   scale_color_manual(values = cols) +
   pthemes +
   #kontrol for box for RHF navn
   theme(strip.background = element_rect(colour = "white", fill = "white"),
         strip.text.x = element_text(colour = "black", face = "bold", size =14)) +
   scale_y_continuous(expand = c(0, 0), breaks = seq(0, maxRN, 20)) +
   scale_x_continuous(breaks = seq(0,110,10)) +
   geom_hline(yintercept = 0, size = 1, color = "black", linetype = "solid") +
   labs(x = "Alder", y = "Antall traume") +
   facet_wrap( ~ RHF))



## 2017 vs 2016
age17 <- data[, list(age17 = .N), by = age]
age16 <- data16[, list(age16 = .N), by = age]

ageMix <- merge(age17, age16, all = TRUE)
bNA(ageMix)

ageLong <- melt(ageMix, id.vars = "age",
                measure.vars = c("age16", "age17"),
                variable.name = "year",
                value.name = "n")

maxN <- max(ageLong$n, na.rm = TRUE)
(antall1617 <- ggplot(ageLong, aes(age, n)) +
   geom_line(aes(group = year, color = year), stat = "identity", size = 0.7) +
   scale_color_manual(breaks = c("age16", "age17"), labels = c("2016","2017"), values = cols2) +
   scale_y_continuous(breaks = seq(0,maxN, 20), expand = c(0, 0)) +
   scale_x_continuous(breaks = seq(0,110,10)) +
   labs(x = "Alder", y = "Antall traume") +
   pthemes)
