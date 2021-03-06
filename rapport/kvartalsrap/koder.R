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
  "kableExtra",
  "DT")

sapply(pakke, require, character.only = TRUE)

source("~/Git-work/traume/traumeApp/setup20181110.R")
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

hospValg <- "Drammen"
datoFra <- "2016-01-01"
datoTil <- "2017-12-31"


## Traumeskjema
masterDT <- masterFile[!duplicated(ntrid) &
                    Hospital == (hospValg) &
                      dateSykehus >= as.POSIXct(datoFra, format = "%Y-%m-%d") &
                       dateSykehus <= as.POSIXct(datoTil, format = "%Y-%m-%d"), ]

antallNTR <- dim(masterDT)[1]

## Traumeskjema
dataDT <- akutt[!duplicated(ntrid) &
                   !is.na(ntrid) &
                   Hospital == (hospValg) &
                    dateSykehus >= as.POSIXct(datoFra, format = "%Y-%m-%d") &
                       dateSykehus <= as.POSIXct(datoTil, format = "%Y-%m-%d"), ]

## Skadeskjema
skadeDT <- skade[!duplicated(ntrid) &
                    !is.na(ntrid) &
                   Hospital == (hospValg) &
                    dateSykehus >= as.POSIXct(datoFra, format = "%Y-%m-%d") &
                       dateSykehus <= as.POSIXct(datoTil, format = "%Y-%m-%d"),]


## Ulykkeskjema
ulykkeDT <- ulykke[!duplicated(ntrid) &
                    !is.na(ntrid) &
                   Hospital == (hospValg) &
                    dateSykehus >= as.POSIXct(datoFra, format = "%Y-%m-%d") &
                       dateSykehus <= as.POSIXct(datoTil, format = "%Y-%m-%d"),]


## prehospitalskjema
prehospDT <- prehosp[!duplicated(ntrid) &
                    !is.na(ntrid) &
                   Hospital == (hospValg) &
                    dateSykehus >= as.POSIXct(datoFra, format = "%Y-%m-%d") &
                       dateSykehus <= as.POSIXct(datoTil, format = "%Y-%m-%d"),]


## Intensiv
intDT <- intensiv[!duplicated(ntrid) &
                    !is.na(ntrid) &
                   Hospital == (hospValg) &
                    dateSykehus >= as.POSIXct(datoFra, format = "%Y-%m-%d") &
                       dateSykehus <= as.POSIXct(datoTil, format = "%Y-%m-%d"),]





## Antall traume
## ===========================
masterDT[!duplicated(ntrid), .N]


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

alarmTab <- dataDT[, .N, by = ed_tta]
alarmTab[, ed_tta := as.character(ed_tta)]
alarmTab[.(ed_tta = c("1", "2"), to = c("Ja", "Nei")), on = "ed_tta", ed_tta := i.to]
data.table::setnames(alarmTab, "ed_tta", "Alarm")

kable(alarmTab, 'latex', booktabs = TRUE)

## Patient age <18
## ===============

age18 <- masterDT[age < 18, .N]

age18sex <- masterDT[age < 18, .N, by = gender][, gender := as.character(gender)]
age18sex[.(gender = c("1", "2"), to = c("Gutter", "Jenter")), on = 'gender', gender := i.to]
data.table::setnames(age18sex, c("gender", "N"), c("", "Antall"))
kable(age18sex, 'latex', booktabs = TRUE)


## Andel NISS > 15 og < 15
## =================
## Bruk skade dataset

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
## Bruker ulykkedataset og skadedataset

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
             "Annet")


ulykkeDT[inj_mechanism %in% c(99,999), inj_mechanism := 88] #Ukjent og Annet til 88
injSum <- ulykkeDT[, .N, by=inj_mechanism]
injSum[, pros := round(N / sum(injSum$N) * 100, digits = 1), by=inj_mechanism]

injMix <- injSum[injSum16, on=c(inj_mechanism = "inj_mechanism")]
injMix[, navn := factor(inj_mechanism,
                        levels = injInd,
                        labels = injNavn)]


## Transport til sykehus
## =====================
## Prehospital skjema

preTransInd <- c(1:6, 99, 999)
preTransNavn <- c(
  "Bilambulanse",
  "Ambulansehelikopter",
  "Ambulansefly",
  "Fraktet inn av publikum",
  "Til sykehus selv",
  "Politi",
  "Annet",
  "Ukjent"
  )

prehospTab <- prehospDT[, .N, by = pre_transport][order(-N)]
prehospTab[, `:=` (
  pros = round(N / sum(N, na.rm = TRUE) * 100, digits = 1),
  trans = factor(pre_transport,
    levels = preTransInd,
    labels = preTransNavn))]


preTransTab <- prehospTab[, .(Transport = trans, Antall = N, Andell = pros)]

preTTot <- preTransTab[, .(Transport = "Total", Antall = sum(Antall, na.rm = T))][,
  Andell := round(Antall / sum(Antall, na.rm = TRUE) * 100, digits = 1)]

preTUT <- rbindlist(list(preTransTab, preTTot))

lastRow <- nrow(preTUT)

kable(preTUT, 'latex', booktabs = TRUE) %>%
  kable_styling(latex_options = "striped") %>%
  row_spec(lastRow, bold = TRUE)


## Ukedager
## ============
## pass på riktig rekkefølge
ukeDagInd <- 1:7
ukeDagNavn <- c("mandag","tirsdag","onsdag","torsdag","fredag","lørdag","søndag")

ukeDag <- dataDT[!is.na(ed_arrival_weekday) & ed_arrival_weekday %in% 1:7,
  .N, by = ed_arrival_weekday]

ukeDag[, `:=` (
  pros = round(N / sum(N, na.rm = TRUE) * 100, digits = 1),
  dag = factor(ed_arrival_weekday, levels = ukeDagInd, labels = ukeDagNavn)
)][, name := sprintf("%s \n (N=%s)", dag, N)]

ukeDag$name <- with(ukeDag, factor(name, levels = name[order(ed_arrival_weekday)]))

col <- '#2171b5'

barTheme <- theme(axis.text = element_text(size = 9, color = "black"), #text for x og y axis
                  axis.ticks.y = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.line.x = element_line(size = 0.5),
                  axis.line.y = element_blank(),
                  axis.title.y = element_text(size = 11),
                  axis.title.x = element_blank(),
                  panel.background = element_rect(fill = "white"),
                  panel.border = element_rect(linetype = 1, fill = NA, color = "white"),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_line(linetype = 2, color = "grey"),
                  legend.position = "none"
                  )

traumeUke <- ggplot(ukeDag, aes(name, pros)) +
  geom_bar(stat = "identity", fill = col, width = .80) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .05))) + #5% space on top
  geom_text(aes(label = pros), vjust = -0.5, position = position_dodge(width = .80)) +
  ylab("prosent") +
  ## geom_text(aes(y = 0.5, label = paste0("N=", n))) +
  barTheme


## Overflyttet traumesenter
## ========================



## Antall liggedøgn
## ================

liggTab <- intDT[!duplicated(ntrid), .N, by = hosp_icu_days]

data.table::setnames(liggTab, c("hosp_icu_days", "N"), c("Antall Dager", "Antal traume"))

kable(liggTab, 'latex', booktabs = TRUE) %>%
  kable_styling(latex_options = "striped")
