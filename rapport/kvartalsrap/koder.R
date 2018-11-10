## DATA
##========

pakke <- c("shiny",
  "shinyBS",
  "shinydashboard",
  "data.table",
  "ggplot2",
  "dygraphs",
  "xts",
  "zoo",
  "knitr",
  "DT")

sapply(pakke, require, character.only = TRUE)

source("~/Git-work/traume/traumeApp/data2.R")
source("~/Git-work/traume/traumeApp/functions/byttNA.R") #bNA()

## ==============
## Function
## ==============

fun.tab <- function(data, var, include){
  DT <- data[!is.na(get(var)) & get(var) %in% include, ]
  all <- DT[, .(N = .N)]
  sub <- DT[, .(n = .N), by = get(var)]

}


## ===============
## Data valg
## ===============

valgSyk <- "Drammen"
datoFra <- "2016-01-01"
datoTil <- "2017-12-31"

dataDT <- akutt[!duplicated(ntrid) &
                   !is.na(ntrid) &
                   Hospital == (valgSyk) &
                    dateSykehus >= as.POSIXct(datoFra, format = "%Y-%m-%d") &
                       dateSykehus <= as.POSIXct(datoTil, format = "%Y-%m-%d"), ]






## Antall traume
## ===========================
dataDT[!duplicated(ntrid), .N]


## Antall traume, alder og kjønn
## =============================
trman <- dataDT[gender == 1, .(Menn = .N), by = age]
trwoman <- dataDT[gender == 2, .(Kvinner = .N), by = age]
trMW <- merge(trman, trwoman, by = "age", all = TRUE)
## Tar bort Alder med NA  og -1
rensTR <- trMW[!is.na(age) & age != -1, ]

bNA(rensTR)

rensTR[, Begge := Menn + Kvinner, by = age]
data.table::setnames(rensTR, "age", "Alder")

dataLong <- melt(rensTR, id.vars = "Alder",
  measure.vars = c("Menn", "Kvinner", "Begge"),
  variable.name = "gender", value.name = "n")


cols <- c("#084594","#6baed6", "#FF7260")
pthemes <- theme(axis.text = element_text(size = 9, color = "black"), #text for x og y axis
                 axis.ticks.y = element_blank(),
                 axis.line.x = element_line(size = 0.5),
                 axis.line.y = element_blank(),
                 axis.title.y = element_text(size = 11),
                 axis.title.x = element_text(size = 11),
                 panel.background = element_rect(fill = "white"),
                 panel.border = element_rect(linetype = 1, fill = NA, color = "white"),
                 panel.grid.minor.x = element_blank(),
                 panel.grid.major.y = element_line(linetype = 2, color = "grey"),
                 legend.position = "bottom",
                 legend.justification = c(0,1), #legend bottom left
                 legend.title = element_blank(),
                 legend.text = element_text(size = 9),
                 legend.key = element_rect(fill = "white")
                 )

maxRN <- max(dataLong$n)
antallTraume <- ggplot(dataLong) +
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
  labs(x = "Alder", y = "Antall traume")

## Traume med eller uten alarm
## ===========================

dataDT[, .N, by = ed_tta]


## Patient age <18
## ===============

dataDT[age < 18, .N]


## Andel NISS > 15 og < 15
## =================

skadeDT <- skade[!duplicated(ntrid) &
                    !is.na(ntrid) &
                   Hospital == (valgSyk) &
                    dateSykehus >= as.POSIXct(datoFra, format = "%Y-%m-%d") &
                       dateSykehus <= as.POSIXct(datoTil, format = "%Y-%m-%d"),]

nissSum <- skadeDT[, .(niss = ifelse(inj_niss < 15, 1, 2))]

nissTab <- nissSum[, .N, by = niss]
nissTab[, niss := as.character(niss)]
nissTab[.(niss = c("1", "2"), to = c("Ja", "Nei")), on = "niss", niss := i.to]

nisTot <- nrow(nissSum)

nissUT <- rbindlist(list(nissTab, list("Total", nisTot)))
nissUT[, Andel := round(N / nisTot * 100, digits = 1), by = niss]
data.table::setnames(nissUT, c("niss", "N"), c("NISS < 15", "Antall"))

## ISS > 15
## ========
issSum <- skadeDT[, .(iss = ifelse(inj_iss < 15, 1, 2))]

issTab <- issSum[, .N, by = iss]
issTab[, iss := as.character(iss)]
issTab[.(iss = c("1", "2"), to = c("Ja", "Nei")), on = "iss", iss := i.to]

issTot <- nrow(issSum)
issUT <- rbindlist(list(issTab, list("Total", issTot)))
issUT[, Andel := round(N / issTot * 100, digits = 1), by = iss]

data.table::setnames(issUT, c("iss", "N"), c("ISS < 15", "Antall"))

## ISS og tatt i mot med traumeteam
## ================================
issTTA <- dataDT[, list(ntrid, ed_tta)]
ttaISS <- skadeDT[, list(ntrid, inj_iss)]

issttaDT <- merge(issTTA, ttaISS, by = "ntrid", all = TRUE)

## tatt imot iss < 15
under15iss <- issttaDT[inj_iss < 15 & ed_tta == 1, .N]

## tatt imot iss > 15
over15iss <- issttaDT[inj_iss > 15 & ed_tta == 1, .N]

tot15iss <- under15iss + over15iss

issTraTab <- data.table(ISS = c("ISS < 15", "ISS > 15", "Total"),
  n = c(under15iss, over15iss, tot15iss))

issTraTab[, pros := round(n / tot15iss * 100, digits = 1), by = ISS]


## TESTING
issttaDT[inj_iss < 15, iss15under := as.numeric(ifelse(ed_tta == 1, 1, 0)), by = ntrid]
issttaDT[, .N, by = iss15under]

issttaDT[inj_iss > 15, iss15over := as.numeric(ifelse(ed_tta == 1, 1, 0)), by = ntrid]
issttaDT[, .N, by = iss15over]

issttaDT[, .N, by = ed_tta]


############################
## Skade mekanismen
############################

injInd <- c(1:11, 88)
## bruk ";" for å dele tekst til 2 linjer når lager plot
injNavn <- c("Trafikk: ulykke med motorkjøretøy; – ikke motorsykkel",
             "Trafikk: motorsykkelulykke",
             "Trafikk: sykkelulykke",
             "Trafikk: fotgjenger",
             "Trafikk: annet",
             "Skutt av håndvåpen: hagle, rifle; eller annen type skytevåpen",
             "Stukket av kniv, sverd, dolk,; andre skarpe eller spisse objekter",
             "Truffet av eller slått; med stumpe objekt",
             "Lavenergi fall",
             "Høyenergi fall",
             "Eksplosjonsskade",
             "Annet"
)

#2017
dataUL[inj_mechanism %in% c(99,999), inj_mechanism := 88] #Ukjent og Annet til 88
injSum <- dataUL[, .N, by=inj_mechanism]
injSum[, pros := round(N / sum(injSum$N) * 100, digits = 1), by=inj_mechanism]
#2016
dataUL16[inj_mechanism %in% c(99,999), inj_mechanism := 88] #Ukjent og Annet til 88
injSum16 <- dataUL16[, .N, by=inj_mechanism]
injSum16[, pros := round(N / sum(injSum16$N) * 100, digits = 1), by=inj_mechanism]

injMix <- injSum[injSum16, on=c(inj_mechanism = "inj_mechanism")]
injMix[, navn := factor(inj_mechanism,
                        levels = injInd,
                        labels = injNavn)]

# plotting
## bruk funksjon rapbar
#########################

fig1 <- rapbar(injMix, navn, N, i.N, pros, i.pros, line2 = TRUE, lpos=0.96)
