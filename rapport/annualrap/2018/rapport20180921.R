## Årsrapport 2018
rm(list = ls())

pkg <- c("data.table", "ggplot2", "directlabels", "cowplot", "gridExtra", "grid")

sapply(pkg, require, character.only = TRUE)

## hent data
source("K:/Sensitivt/kvalitetsregistre/2013-9541_Nasjonalt_traumeregister/Yusman/rapport/2018/setup.R")

## Function
source("K:\\Sensitivt\\kvalitetsregistre\\2013-9541_Nasjonalt_traumeregister\\Yusman\\rapport\\2018\\function\\rapbar.R")
source("K:\\Sensitivt\\kvalitetsregistre\\2013-9541_Nasjonalt_traumeregister\\Yusman\\rapport\\2018\\function\\byttna.R")

## sted å lage figurer
savefig <- "K:\\Sensitivt\\kvalitetsregistre\\2013-9541_Nasjonalt_traumeregister\\Yusman\\rapport\\2018\\fig"


## Prepare data TRAUME Skjema
################################
## Valg muligheter
datoFra <- "2017-01-01"
datoTil <- "2017-12-31"

datoFra01 <- "2016-01-01"
datoTil01 <- "2016-12-31"

## henter data
source("./valgdata.R")


## Felles paramenters
cols4 <- c("#4292c6", "#c6dbef", "#FF7260", "#084594")
cols <- c("#084594","#6baed6", "#FF7260")
cols2 <- c("#FF7260", "#2171b5")
cols1 <- "#4292c6"
col1 <- "#6baed6"
col2 <- "#2171b5" #hvis bare en søyle
col3 <- "#084594" #Den andre søyle


######################################################################################
###################  ANALYSER  #######################################################
######################################################################################


##################
## Antall traume
##################
## regional deling
ageMan <- data[gender == 1, list(mann = .N), by = list(RHF, age)]
ageKvinne <- data[gender == 2, list(kvinne = .N), by = list(RHF, age)]
ageMK <- merge(ageMan, ageKvinne, all = TRUE)


## bytt NA med 0
byttNA(ageMK)

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
  labs(x = "Alder", y = "Antall traume") +
  facet_wrap( ~ RHF)



## save file generic
fig1 <- antallTraume
title <- "fig1_antall_regionvis"

## Save figure ================================
fig1a <- ggplot_gtable(ggplot_build(fig1))
fig1a$layout$clip[fig1a$layout$name == 'panel'] <- 'off'
grid.draw(fig1a)
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
## ggsave("~/Git-work/HSR/arsrapport/fig1a.jpg")
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL




## Antall traume 2017 vs 2016
#############################
age17 <- data[, list(age17 = .N), by = age]
age16 <- data16[, list(age16 = .N), by = age]

ageMix <- merge(age17, age16, all = TRUE)
byttNA(ageMix)

ageLong <- melt(ageMix, id.vars = "age",
                measure.vars = c("age16", "age17"),
                variable.name = "year",
                value.name = "n")

maxN <- max(ageLong$n, na.rm = TRUE)
antall1617 <- ggplot(ageLong, aes(age, n)) +
  geom_line(aes(group = year, color = year), stat = "identity", size = 0.7) +
  scale_color_manual(breaks = c("age16", "age17"),
                     labels = c("2016","2017"), values = cols2) +
  scale_y_continuous(breaks = seq(0,maxN, 20), expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0,110,10)) +
  labs(x = "Alder", y = "Antall traume") +
  pthemes



## save file generic
fig1 <- antall1617
title <- "fig2_antall_traume"

## Save figure ================================
fig1a <- ggplot_gtable(ggplot_build(fig1))
fig1a$layout$clip[fig1a$layout$name == 'panel'] <- 'off'
grid.draw(fig1a)
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
## ggsave("~/Git-work/HSR/arsrapport/fig1a.jpg")
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL






###############
## AKUTT DATA
###############
## 3. Antall og ukedag
########################


valgDato <- dataAK[!duplicated(ntrid) & !is.na(dateSykehus)] #ed_arrival_dtg
valgDag <- valgDato[, dag := weekdays(dateSykehus)]
ntot <- dim(valgDag)[1] #total
ukeDag <- valgDag[, .(pros = round((.N / ntot) * 100, digits = 1),
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
  geom_bar(stat = "identity", fill = col2, width = .80) +
  scale_y_continuous(expand = expand_scale(mult = c(0, .05))) + #5% space on top
  geom_text(aes(label = pros), vjust = -0.5, position = position_dodge(width = .80)) +
  ylab("prosent") +
  ## geom_text(aes(y = 0.5, label = paste0("N=", n))) +
  barTheme


## save file generic
fig1 <- traumeUke
title <- "fig3_ukerdager"

## Save figure ================================
fig1a <- ggplot_gtable(ggplot_build(fig1))
fig1a$layout$clip[fig1a$layout$name == 'panel'] <- 'off'
grid.draw(fig1a)
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
## ggsave("~/Git-work/HSR/arsrapport/fig1a.jpg")
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL





###################
## ULYKKE SKJEMA ##
###################
## 4. Ulykketyper
#################
## Prepare data TRAUME Skjema
################################
# 
# ## 2017 Data
# dataRawUl <- ulykke[dateAll >= as.POSIXct(datoFra, format = "%Y-%m-%d") &
#                       dateAll <= as.POSIXct(datoTil, format = "%Y-%m-%d")]
# 
# 
# ## bort med NA og duplicated - ntrid, alder og kjønn
# dataUL <- dataRawUl[!is.na(ntrid) &
#                       !duplicated(ntrid) &
#                    !is.na(age) &
#                    age != -1 &
#                    !is.na(gender)]
# 
# 
# ## 2016 Data
# dataRawUL16 <- ulykke[dateAll >= as.POSIXct(datoFra01, format = "%Y-%m-%d") &
#                         dateAll <= as.POSIXct(datoTil01, format = "%Y-%m-%d")]
# 
# 
# ## bort med NA og duplicated - ntrid, alder og kjønn
# dataUL16 <- dataRawUL16[!is.na(ntrid) &
#                           !duplicated(ntrid) &
#                        !is.na(age) &
#                        age != -1 &
#                        !is.na(gender)]

## Liste over ulykke typer
navnUT <- c("acc_transport",
            "acc_fall",
            "acc_violence",
            "acc_self_inflict",
            "acc_work",
            "acc_sprt_recreat",
            "acc_fire_inhal",
            "acc_other")

navnIN <- c("Transport",
            "Fall",
            "Vold",
            "Selvpåført",
            "Arbeid",
            "Sport- og fritid",
            "Brann- og inhalasjon",
            "Annen ulykke"
            )

## Funksjon for telling kolonne
tellCol <- function(x = dataUL, col, value = 1){
  require(data.table)
  setDT(x)
  valg1 = x[get(col) == value, .N]
  valg2 = x[, .(N = ifelse(!is.na(get(col)), 1, 0))][, sum(N, na.rm = TRUE)]
  data <- data.table(var = col, n = valg1, N = valg2)
  data[, prosent := round((n / N) * 100, digits = 1)]
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
## Gir riktig navn til ulykke typer
accMix[, navn := factor(var, levels = navnUT, labels = navnIN)]

#### Plotting
#### bruk funksjon rapbar
#########################

fig1 <- rapbar(accMix, navn, n, i.n, prosent, i.prosent)
title <- "fig4_ulykketyper"

fig1a <- fig1
grid.draw(fig1a)
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
## ggsave("~/Git-work/HSR/arsrapport/fig1a.jpg")
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL




#########################
## Transport
########################

## Omkode Annet/ukjent og MC/Moped
dataUL[, transkode := acc_trsp_rd_type]
dataUL[acc_trsp_rd_type == 99 | acc_trsp_rd_type == 999, transkode := 9] #annen/ukjent
dataUL[acc_trsp_rd_type == 2 | acc_trsp_rd_type == 7, transkode := 2] #moped og mc

dataUL16[, transkode := acc_trsp_rd_type]
dataUL16[acc_trsp_rd_type == 99 | acc_trsp_rd_type == 999, transkode := 9] #annen/ukjent
dataUL16[acc_trsp_rd_type == 2 | acc_trsp_rd_type == 7, transkode := 2] #moped og mc

## trans2017
trans <- dataUL[transkode != 0, .N, by = transkode] #ikke valgt er ut
trans[, pros := round(N / sum(trans$N) * 100, digits = 1), by = transkode]

## trans2016
trans16 <- dataUL16[transkode != 0, .N, by = transkode] #ikke valgt er ut
trans16[, pros := round(N / sum(trans16$N) * 100, digits = 1), by = transkode]

## merge got get masterfil (MF)
transMF <- trans[trans16, on = c(transkode = "transkode")]

## Gir navn
transNavn <- c("Bil", "MC/Moped", "Sykkel", "Båt", "Tog", "Fly", "Annet/Ukjent")
transInd <- c(1:6,9)

transMF[, navn := factor(transkode,
                         levels = transInd,
                           labels = transNavn)]


## plotting
## bruk funksjon rapbar
#########################

fig1 <- rapbar(transMF, navn, N, i.N, pros, i.pros)
title <- "fig5_transport_type"

fig1a <- fig1
grid.draw(fig1a)
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
## ggsave("~/Git-work/HSR/arsrapport/fig1a.jpg")
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


## Fig 6 - ISS > 15
######################
# skade[, iss15 := ifelse(inj_iss>15, 1, 0), by=seq_len(nrow(skade))]

## 2017
issData <- dataSK[!duplicated(ntrid), list(ntrid, iss15)]
ulType <- dataUL[!duplicated(ntrid), list(ntrid, acc_trsp_rd_type, transkode)]
issUL <- ulType[issData, on=c(ntrid="ntrid")]

plotISSraw <- issUL[iss15==1, .N, by=transkode]
plotISSraw[, navn := factor(transkode,
                           levels = transInd,
                           labels = transNavn)]

plotISS <- plotISSraw[!is.na(navn), ]
plotISS[, pros := round(N / sum(plotISS$N) * 100, digits = 1), by = transkode]

## 2016
issData16 <- dataSK16[!duplicated(ntrid), list(ntrid, iss15)]
ulType16 <- dataUL16[!duplicated(ntrid), list(ntrid, acc_trsp_rd_type, transkode)]
issUL16 <- ulType16[issData16, on=c(ntrid="ntrid")]

plotISSraw16 <- issUL16[iss15==1, .N, by=transkode]
plotISSraw16[, navn := factor(transkode,
                            levels = transInd,
                            labels = transNavn)]

plotISS16 <- plotISSraw16[!is.na(navn),]
plotISS16[, pros := round(N / sum(plotISS16$N) * 100, digits = 1), by = transkode]

# Mix begge
issMix <- plotISS[plotISS16, on=c(transkode = "transkode")]


## plotting
## bruk funksjon rapbar
#########################

fig1 <- rapbar(issMix, navn, N, i.N, pros, i.pros)
title <- "fig6_iss15"

fig1a <- fig1
grid.draw(fig1a)
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
## ggsave("~/Git-work/HSR/arsrapport/fig1a.jpg")
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL



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
dd <- data.table(name = cc, n = valg1, N = valg2)
dd[, pros := round((n / N) * 100)]
dd

tellColtest <- function(x, col, value = 1){
  require(data.table)
  setDT(x)
  valg1 = x[get(col) == value, .N]
  valg2 = x[, .(N = ifelse(!is.na(get(col)), 1, 0))][, sum(N, na.rm = TRUE)]
  dd <- data.table(var = col, n = valg1, N = valg2)
  dd[, prosent := round((n / N) * 100)]
  return(dd)
}

testUT <- tellColtest(DT, "col1")
