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


## Prepare data TRAUME Skjema
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

cols <- c("#4292c6", "#c6dbef", "#FF7260", "#084594")
cols2 <- c("#FF7260", "#2171b5")
cols1 <- "#4292c6"

pthemes <- theme(axis.text = element_text(size = 9, color = "black"), #text for x og y axis
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



## Antall traume 2017 vs 2016
#############################
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
   scale_color_manual(breaks = c("age16", "age17"),
                      labels = c("2016","2017"), values = cols2) +
   scale_y_continuous(breaks = seq(0,maxN, 20), expand = c(0, 0)) +
   scale_x_continuous(breaks = seq(0,110,10)) +
   labs(x = "Alder", y = "Antall traume") +
   pthemes)


###############
## AKUTT DATA
###############
## 3. Antall og ukedag
########################

## 2017 data
dataRawAK <- akutt2[dateSykehus >= as.Date(datoFra, format = "%Y-%m-%d") &
                      dateSykehus <= as.Date(datoTil, format = "%Y-%m-%d")]


## bort med NA og duplicated - ntrid, alder og kjønn
dataAK <- dataRawAK[!is.na(ntrid) &
                      !duplicated(ntrid) &
                   !is.na(age) &
                   age != -1 &
                   !is.na(gender)]


valgDato <- dataAK[!duplicated(ntrid) & !is.na(dateSykehus)] #ed_arrival_dtg
valgDag <- valgDato[, dag := weekdays(dateSykehus)]
ntot <- dim(valgDag)[1] #total
ukeDag <- valgDag[, .(pros = round((.N / ntot) * 100),
                      n = .N), by = dag]

## pass på riktig rekkefølge
ukeDag$dagnr <- factor(ukeDag$dag,
                       levels = c("mandag", "tirsdag", "onsdag", "torsdag",
                                  "fredag", "lørdag", "søndag"),
                     labels = 1:7)

ukeDag$name <- sprintf("%s \n (N=%s)", ukeDag$dag, ukeDag$n)
ukeDag$name <- with(ukeDag, factor(name, levels = name[order(dagnr)]))

barTheme <- theme(axis.text = element_text(size = 9, color = "black"), #text for x og y axis
                  axis.ticks.y = element_blank(),
                  axis.ticks.x = element_blank(),
                  axis.line.x = element_line(size = 0.5),
                  axis.title.y = element_text(size = 11),
                  axis.title.x = element_blank(),
                  panel.background = element_rect(fill = "white"),
                  panel.border = element_rect(linetype = 1, fill = NA, color = "white"),
                  panel.grid.minor.x = element_blank(),
                  panel.grid.major.y = element_line(linetype = 2, color = "grey"),
                  legend.position = "none"
                  )

traumeUke <- ggplot(ukeDag, aes(name, pros)) +
  geom_bar(stat = "identity", fill = cols1, width = .80) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .05))) + #5% space on top
  geom_text(aes(label = pros), vjust = -0.5, position = position_dodge(width = .80)) +
  ylab("prosent") +
  ## geom_text(aes(y = 0.5, label = paste0("N=", n))) +
  barTheme

traumeUke



###################
## ULYKKE SKJEMA ##
###################
## 4. Ulykketyper
#################
## Prepare data TRAUME Skjema
################################

## 2017 Data
dataRawUl <- ulykke[dateAll >= as.Date(datoFra, format = "%Y-%m-%d") &
                      dateAll <= as.Date(datoTil, format = "%Y-%m-%d")]


## bort med NA og duplicated - ntrid, alder og kjønn
dataUL <- dataRawUl[!is.na(ntrid) &
                      !duplicated(ntrid) &
                   !is.na(age) &
                   age != -1 &
                   !is.na(gender)]


## 2016 Data
dataRawUL16 <- ulykke[dateAll >= as.Date(datoFra01, format = "%Y-%m-%d") &
                        dateAll <= as.Date(datoTil01, format = "%Y-%m-%d")]


## bort med NA og duplicated - ntrid, alder og kjønn
dataUL16 <- dataRawUL16[!is.na(ntrid) &
                          !duplicated(ntrid) &
                       !is.na(age) &
                       age != -1 &
                       !is.na(gender)]

## Liste over ulykke typer
navnUT <- c("acc_transport",
            "acc_fall",
            "acc_violence",
            "acc_self_inflict",
            "acc_work",
            "acc_sprt_recreat",
            "acc_fire_inhal",
            "acc_other")

## Funksjon for telling kolonne
tellCol <- function(x = dataUL, col, value = 1){
  require(data.table)
  setDT(x)
  valg1 = x[get(col) == value, .N]
  valg2 = x[, .(N = ifelse(!is.na(get(col)), 1, 0))][, sum(N, na.rm = TRUE)]
  data <- data.table(var = col, n = valg1, N = valg2)
  data[, prosent := round((n / N) * 100)]
  return(data)
}

trans <- tellCol(col = "acc_transport")
fall <- tellCol(col = "acc_fall")
violence <- tellCol(col = "acc_violence")
self <- tellCol(col = "acc_self_inflict")
work <- tellCol(col = "acc_work")
sport <- tellCol(col = "acc_sprt_recreat")
fire <- tellCol(col = "acc_fire_inhal")
other <- tellCol(col = "acc_other")

accData <- rbindlist(list(trans, fall, violence, self, work, sport, fire, other))

## 2016 ulykke typer

trans16 <- tellCol(x = dataUL16, col = "acc_transport")
fall16 <- tellCol(x = dataUL16, col = "acc_fall")
violence16 <- tellCol(x = dataUL16, col = "acc_violence")
self16 <- tellCol(x = dataUL16, col = "acc_self_inflict")
work16 <- tellCol(x = dataUL16, col = "acc_work")
sport16 <- tellCol(x = dataUL16, col = "acc_sprt_recreat")
fire16 <- tellCol(x = dataUL16, col = "acc_fire_inhal")
other16 <- tellCol(x = dataUL16, col = "acc_other")

accData16 <- rbindlist(list(trans16, fall16, violence16, self16, work16, sport16, fire16, other16))

## Merge
accMix <- accData[accData16, on = c(var = "var")] #i.n, i.N og i.prosent er for 2016 dvs i = inne i mergeing


#### Plot
data <- accMix[order(accMix$prosent, decreasing = TRUE),]
data[, ref := seq.int(nrow(accMix))]

## text til tabell
data[, text1:= paste0(n, " (", prosent, "%)"), by=var]
data[, text2:= paste0(i.n, " (", i.prosent, "%)"), by=var]

## New column for reference og dummy
dfrow <- nrow(data)
data$ref <- seq.int(dfrow)

## create dummy row for text - ref highest row and "" for var to avoid showing NA in
## x-axis
data <- rbindlist(list(data, data[NA]))
data[is.na(ref), `:=` (var = "", ref = dfrow + 1)]

ymax <- max(data$prosent, na.rm = TRUE)
ylocal <- "prosent"
ycomp = "i.prosent"
ref = "ref"
leg1 = "2017"
leg2 = "2016"
col1 = "blue"
col2 = "orange"

p <- ggplot(data) +
  geom_segment(aes(x = ref, y = ymax, xend = ref, yend = 0),
               size = 0.3, linetype = 2, color = "grey70") +
  ## 'fill' is used to get legend for geom_bar
  geom_bar(aes_string(ref, ylocal, fill = leg1), stat = "identity") +
  ## 'color' is used to get legend
  geom_point(aes_string(ref, ycomp, color = leg2), stat = "identity",
             shape = 18, size = 6) +
  coord_flip() +
  scale_x_discrete(breaks = factor(data$ref), labels = data$var) +
  scale_fill_manual(values = col1) + #for bar
  scale_color_manual(values = col2) + #for point
  ## order in guides to specify order of the legend and not alphabetically
  guides(fill = guide_legend(override.aes = list(shape = NA), order = 1))





library(rreg)
regbar(accData, var, prosent, num = "n")






##############
## TEST DATA
###############
DT <- data.table(col1 = c(rep(1:4, 2), NA, NA), col2 = rep(1:2, 5), col3 = rep(1,10))
DT

cc <- "col1"
val <- 1
valg1 <- DT[get(cc) == val, .N]
valg1
valg2 <- DT[, list(N = ifelse(!is.na(get(cc)), 1, 0))][, sum(N)]
data <- data.table(name = cc, n = valg1, N = valg2)
data[, pros := round((n / N) * 100)]
data

tellColtest <- function(x, col, value = 1){
  require(data.table)
  setDT(x)
  valg1 = x[get(col) == value, .N]
  valg2 = x[, .(N = ifelse(!is.na(get(col)), 1, 0))][, sum(N, na.rm = TRUE)]
  data <- data.table(var = col, n = valg1, N = valg2)
  data[, prosent := round((n / N) * 100)]
  return(data)
}

testUT <- tellColtest(DT, "col1")
