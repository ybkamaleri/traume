## Årsrapport 2018
rm(list = ls())

pkg <- c("data.table", "ggplot2", "directlabels", "cowplot", "gridExtra", "grid")

sapply(pkg, require, character.only = TRUE)

## hent data
source("K:/Sensitivt/kvalitetsregistre/2013-9541_Nasjonalt_traumeregister/Yusman/rapport/2018/setup.R")

## Function
source("K:\\Sensitivt\\kvalitetsregistre\\2013-9541_Nasjonalt_traumeregister\\Yusman\\rapport\\2018\\function\\rapbar.R")
source("K:\\Sensitivt\\kvalitetsregistre\\2013-9541_Nasjonalt_traumeregister\\Yusman\\rapport\\2018\\function\\rapbargg.R")
source("K:\\Sensitivt\\kvalitetsregistre\\2013-9541_Nasjonalt_traumeregister\\Yusman\\rapport\\2018\\function\\sharelegend.R")
source("K:\\Sensitivt\\kvalitetsregistre\\2013-9541_Nasjonalt_traumeregister\\Yusman\\rapport\\2018\\function\\byttna.R")
source("K:\\Sensitivt\\kvalitetsregistre\\2013-9541_Nasjonalt_traumeregister\\Yusman\\rapport\\2018\\function\\alderkat.R")

## sted å lage figurer
savefig <- "K:\\Sensitivt\\kvalitetsregistre\\2013-9541_Nasjonalt_traumeregister\\Yusman\\rapport\\2018\\fig"

## Valg muligheter
datoFra <- "2017-01-01"
datoTil <- "2017-12-31"

datoFra01 <- "2016-01-01"
datoTil01 <- "2016-12-31"

## Felles paramenters
cols4 <- c("#4292c6", "#c6dbef", "#FF7260", "#084594")
cols <- c("#084594","#6baed6", "#FF7260")
cols2 <- c("#FF7260", "#2171b5")
cols1 <- "#4292c6"
col1 <- "#6baed6"
col2 <- "#2171b5" #hvis bare en søyle
col3 <- "#084594" #Den andre søyle

## Prepare data TRAUME Skjema
################################
source("K:/Sensitivt/kvalitetsregistre/2013-9541_Nasjonalt_traumeregister/Yusman/rapport/2018/dataFilter.R")


######################################################################################
###################  ANALYSER  #######################################################
######################################################################################


##################
## Antall traume
##################
## regional deling
data[, agekat := alder.kat(age, min(age, na.rm = T), max(age, na.rm = T), by=5)] #lage alderkategorier
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


#################################################################

##########################
## alder kategorier
ageMank <- data[gender == 1, list(mann = .N), by = list(RHF, agekat)]
ageKvinnek <- data[gender == 2, list(kvinne = .N), by = list(RHF, agekat)]
ageMKK <- merge(ageMank, ageKvinnek, all = TRUE)

## bytt NA med 0
byttNA(ageMKK)

## Lager total
ageMKK[, alle := mann + kvinne, by = list(RHF, agekat)]

## lager prosen per alder
ageMKK[, proM := round(mann / alle *100, digits = 1), by=agekat]
ageMKK[, proK := round(kvinne / alle *100, digits = 1), by=agekat]
ageMKK[, sumRHF := sum(alle, na.rm = T), by=RHF]
ageMKK[, proA := round(alle / sumRHF * 100, digits = 1)]
ageMKK[, proAll := round(alle/sum(alle, na.rm = T)*100, digits = 1), by=RHF]

## lager prosen per kjønn
ageMKK[, proM2 := round(mann / sum(mann, na.rm = T) *100, digits = 1), by=RHF]
ageMKK[, proK2 := round(kvinne / sum(kvinne, na.rm = T) *100, digits = 1), by=RHF]
ageMKK[, sumRHF := sum(alle, na.rm = T), by=RHF]
ageMKK[, proA := round(alle / sumRHF * 100, digits = 1)]
ageMKK[, proAll := round(alle/sum(alle, na.rm = T)*100, digits = 1), by=RHF]

## Gir nytt navn
# newNavn <- c("RHF", "Aldersgruppe", "Menn", "Kvinner", "Begge")
# data.table::setnames(ageMKK, 1:5, newNavn)
# ageMKK

## konverterer data til long
ageLongPro <-melt(ageMKK, id.vars=c("RHF", "agekat"),
                   measure.vars=c("proM","proK","proA"),
                   variable.name="gender", value.name="pros")


## konverterer data til long per agekat HF
ageLongPro2 <-melt(ageMKK, id.vars=c("RHF", "agekat"),
                  measure.vars=c("proM2","proK2","proA"),
                  variable.name="gender", value.name="pros")

maxRN <- max(ageLongPro$pros)
proTraumeAge <- ggplot(ageLongPro) +
  geom_point(aes(x = agekat, y = pros, group = gender, color = gender), size = 1) +
  geom_line(aes(x = agekat, y = pros, group = gender, color = gender), size = 1) +
  ## scale_linetype_manual(values = c(1,1,4)) +
  scale_color_manual(values = cols, labels=c("Menn", "Kvinner", "Andel aldersgruppe for begge")) +
  pthemes +
  #kontrol for box for RHF navn
  theme(strip.background = element_rect(colour = "white", fill = "white"),
        strip.text.x = element_text(colour = "black", face = "bold", size =14)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, maxRN, 20)) +
  # scale_x_continuous(breaks = seq(0,110,10)) +
  geom_hline(yintercept = 0, size = 1, color = "black", linetype = "solid") +
  labs(x = "Alderskategori", y = "Prosent") + coord_flip() +
  theme(panel.spacing = unit(2, "lines")) + #gap between faceted plot
  facet_wrap( ~ RHF)

proTraumeAge

### per agekat

maxRN <- max(ageLongPro2$pros)
proTraumeRHF <- ggplot(ageLongPro2) +
  geom_line(aes(x = agekat, y = pros, group = gender, color = gender), size = 1) +
  ## scale_linetype_manual(values = c(1,1,4)) +
  scale_color_manual(values = cols, labels=c("Menn", "Kvinner", "Begge")) +
  pthemes +
  #kontrol for box for RHF navn
  theme(strip.background = element_rect(colour = "white", fill = "white"),
        strip.text.x = element_text(colour = "black", face = "bold", size =14)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(0, maxRN, 20)) +
  # scale_x_continuous(breaks = seq(0,110,10)) +
  geom_hline(yintercept = 0, size = 1, color = "black", linetype = "solid") +
  labs(x = "Alderskategori", y = "Prosent") + coord_flip() +
  theme(panel.spacing = unit(2, "lines")) + #gap between faceted plot
  facet_wrap( ~ RHF)

proTraumeRHF


## save file generic
fig1 <- proTraumeAge
title <- "fig1_prosen_regionvis"

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


## save file generic
fig1 <- proTraumeRHF
title <- "fig1a_prosenHF_regionvis"

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


############################################################################
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

#### Alternativ 2 Andel
data16[, agekat := alder.kat(age, min(age, na.rm = T), max(age, na.rm = T), by=5)] #lage alderkategorier

## prosen for norge #####
ageManNo <- data[gender == 1, list(mann = .N), by = list(agekat)]
ageKvinneNo <- data[gender == 2, list(kvinne = .N), by = list(agekat)]
ageNo <- merge(ageManNo, ageKvinneNo, all = TRUE)

ageManNo16 <- data16[gender == 1, list(mann = .N), by = list(agekat)]
ageKvinneNo16 <- data16[gender == 2, list(kvinne = .N), by = list(agekat)]
ageNo16 <- merge(ageManNo16, ageKvinneNo16, all = TRUE)

## bytt NA med 0
byttNA(ageNo)
byttNA(ageNo16)

## Lager total
ageNo[, alle := mann + kvinne, by = list(agekat)]
## lager prosen
ageNo[, proM := round(mann / alle *100, digits = 1), by=agekat]
ageNo[, proK := round(kvinne / alle *100, digits = 1), by=agekat]
ageNo[, sumRHF := sum(alle, na.rm = T)]
ageNo[, proA := round(alle / sumRHF * 100, digits = 1)]
ageNo[, proAll := round(alle/sum(alle, na.rm = T)*100, digits = 1)]
ageNo[, RHF := "Hele landet"]

## 2016

## Lager total
ageNo16[, alle := mann + kvinne, by = list(agekat)]
## lager prosen
ageNo16[, proM := round(mann / alle *100, digits = 1), by=agekat]
ageNo16[, proK := round(kvinne / alle *100, digits = 1), by=agekat]
ageNo16[, sumRHF := sum(alle, na.rm = T)]
ageNo16[, proA := round(alle / sumRHF * 100, digits = 1)]
ageNo16[, proAll := round(alle/sum(alle, na.rm = T)*100, digits = 1)]
ageNo16[, RHF := "Hele landet"]

ageNo1 <- ageNo[, list(agekat, proA)]
ageNo2 <- ageNo16[, list(agekat, proAll)]
ageNMix <- ageNo1[ageNo2, on=c(agekat="agekat")]


norgeLong <- melt(ageNMix, id.vars = "agekat",
                measure.vars = c("proA", "proAll"),
                variable.name = "year",
                value.name = "pros")

maxNorge <- max(norgeLong$pros, na.rm = TRUE)
prosent1617 <- ggplot(norgeLong, aes(agekat, pros)) +
  geom_point(aes(group = year, color = year), stat = "identity", size = 4) +
  geom_line(aes(group = year, color = year), stat = "identity", size = 0.4) +
  scale_color_manual(breaks = c("proA", "proAll"),
                     labels = c("2016","2017"), values = cols2) +
  scale_y_continuous(breaks = seq(0,maxNorge, 1), expand = c(0, 0)) +
  # scale_x_continuous(breaks = seq(0,110,10)) +
  labs(x = "", y = "Andel traume") +
  pthemes + coord_flip()

prosent1617

## save file generic
fig1 <- prosent1617
title <- "fig2a_prosen_traume_2016_vs_2017"

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

fig1 <- rapbar(accMix, navn, n, i.n, prosent, i.prosent, lpos=0.93)
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
## fig5 - Transport
########################
# 
# ## Omkode Annet/ukjent og MC/Moped
dataUL[, transkode := acc_trsp_rd_type]
# dataUL[acc_trsp_rd_type == 99 | acc_trsp_rd_type == 999, transkode := 9] #annen/ukjent
# dataUL[acc_trsp_rd_type == 2 | acc_trsp_rd_type == 7, transkode := 2] #moped og mc
# 
dataUL16[, transkode := acc_trsp_rd_type]
# dataUL16[acc_trsp_rd_type == 99 | acc_trsp_rd_type == 999, transkode := 9] #annen/ukjent
# dataUL16[acc_trsp_rd_type == 2 | acc_trsp_rd_type == 7, transkode := 2] #moped og mc

## trans2017
trans <- dataUL[!(transkode %in% c(0,999)), .N, by = transkode] #ikke valgt og Ukjent er ut
trans[, pros := round(N / sum(trans$N) * 100, digits = 1), by = transkode]

## trans2016
trans16 <- dataUL16[!(transkode %in% c(0,999)), .N, by = transkode] #ikke valgt og Ukjent er ut
trans16[, pros := round(N / sum(trans16$N) * 100, digits = 1), by = transkode]

## merge got get masterfil (MF)
transMF <- trans[trans16, on = c(transkode = "transkode")]

## Gir navn
transNavn <- c("Bil", "MC", "Sykkel", "Båt", "Tog", "Fly", "Moped", "Annet")
transInd <- c(1:7,99)

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

#######################
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

fig1 <- rapbar(issMix, navn, N, i.N, pros, i.pros, lpos = 0.91)
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


############################
## nr. 7 - Skade mekanismen
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
title <- "fig7_inj_mechanism"

fig1a <- fig1
grid.draw(fig1a)
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


#########################
## nr. 7b - inj_dominan

domInd <- c(1,2,999)
domNavn <- c("Stump", "Penetrerende", "Ukjent")

##2017
domSum <- dataUL[, .N, by=inj_dominant]
domSum[, pros := round(N /sum(domSum$N)*100, digits = 1), by=inj_dominant]
##2016
domSum16 <- dataUL16[, .N, by=inj_dominant]
domSum16[, pros := round(N /sum(domSum16$N)*100, digits = 1), by=inj_dominant]

domMix <- domSum[domSum16, on=c(inj_dominant ="inj_dominant")]
domMix[, navn := factor(inj_dominant,
                        levels = domInd,
                        labels = domNavn)]


# plotting
## bruk funksjon rapbar
#########################

fig1 <- rapbar(domMix, navn, N, i.N, pros, i.pros, line2 = FALSE, lpos=0.85, lgap=9) #lgap er avstand mellom tallet 2016 og 2017
title <- "fig7b_inj_dominant"

fig1a <- fig1
grid.draw(fig1a)
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 5, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 5, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 5, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL



##########################
## fig. 8. GOS endring
##########################

mixULIN <- dataIN[dataUL, on=c(ntrid="ntrid")]
## bort med ikke valg og ukjent for begge
gos <- mixULIN[, list(ntrid=ntrid, pre=pt_gos_preinjury, pos=res_gos_dischg, RHF=RHF)]
gosall <- gos[!is.na(pre) & !is.na(pos) & pre!= 999 & pos!=999 & pre!=0 & pos!=0, ]
gosall[, gos := pos - pre, by=ntrid]

## GOS landet
gosNorge <- gosall[gos>=0, .N, by=gos]
gosNorge[, pros := round(N / sum(gosNorge$N)*100, digits = 1), by=gos]

## Helse Vest
gosVest <- gosall[gos>=0 & RHF=="Helse Vest", .N, by=gos]
gosVest[, pros := round(N / sum(gosVest$N)*100, digits = 1), by=gos]
gosVest2 <- gosVest[gosNorge, on=c(gos = "gos")] #merge med norge
gosVest2[, navn := as.factor(gos)]
byttNA(gosVest2)
## Helse Nord
gosNord <- gosall[gos>=0 & RHF=="Helse Nord", .N, by=gos]
gosNord[, pros := round(N / sum(gosNord$N)*100, digits = 1), by=gos]
gosNord2 <- gosNord[gosNorge, on=c(gos = "gos")] #merge med norge
gosNord2[, navn := as.factor(gos)]
byttNA(gosNord2)
## Helse SØ
gosSO <- gosall[gos>=0 & RHF=="Helse Sør-Øst", .N, by=gos]
gosSO[, pros := round(N / sum(gosSO$N)*100, digits = 1), by=gos]
gosSO2 <- gosSO[gosNorge, on=c(gos = "gos")] #merge med norge
gosSO2[, navn := as.factor(gos)]
## Helse Midt
gosMidt <- gosall[gos>=0 & RHF=="Helse-Midt", .N, by=gos]
gosMidt[, pros := round(N / sum(gosMidt$N)*100, digits = 1), by=gos]
gosMidt2 <- gosMidt[gosNorge, on=c(gos = "gos")] #merge med norge
gosMidt2[, navn := as.factor(gos)]

source("K:\\Sensitivt\\kvalitetsregistre\\2013-9541_Nasjonalt_traumeregister\\Yusman\\rapport\\2018\\function\\rabVest.R")
vest <- rapvest(gosVest2, navn, N, i.N, pros, i.pros, lpos=0.84,lgap=9)
source("K:\\Sensitivt\\kvalitetsregistre\\2013-9541_Nasjonalt_traumeregister\\Yusman\\rapport\\2018\\function\\rapNord.R")
nord <- rapNord(gosNord2, navn, N, i.N, pros, i.pros, lpos=0.84,lgap=9)
source("K:\\Sensitivt\\kvalitetsregistre\\2013-9541_Nasjonalt_traumeregister\\Yusman\\rapport\\2018\\function\\rapSO.r")
sor <- rapSO(gosSO2, navn, N, i.N, pros, i.pros, lpos=0.84,lgap=9)
source("K:\\Sensitivt\\kvalitetsregistre\\2013-9541_Nasjonalt_traumeregister\\Yusman\\rapport\\2018\\function\\rapMidt.R")
midt <- rapMidt(gosMidt2, navn, N, i.N, pros, i.pros, lpos=0.84,lgap=9)


# plotting
## bruk funksjon rapbar
#########################

## VEST ##################
title <- "fig8_GOS_Vest"

fig1a <- vest
grid.draw(fig1a)
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL

## Nord #####################
title <- "fig8_GOS_Nord"

fig1a <- nord
grid.draw(fig1a)
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL

## Sør ##################
title <- "fig8_GOS_SorOst"

fig1a <- sor
grid.draw(fig1a)
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL

## Midt ##############
title <- "fig8_GOS_Midt"

fig1a <- midt
grid.draw(fig1a)
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL



#############################################################################################################
## Fig 9a - ISS > 15 hele landet og per HF, RHF og sykehus

## Landet
issNorge <- dataSK[, .N, by=iss15]
issNorge[, RHF := "Hele landet"]
issNorge[, tot := sum(N)]
issNorge[, pros := round(N / sum(issNorge$N)*100, digits = 1), by=iss15]

## RHF
issRHF <- dataSK[, .N, by=list(RHF, iss15)]
issRHF[, tot := sum(N), by=list(RHF)]
issRHF[, pros := round(N / tot*100, digits = 1), by=.(RHF, iss15)]
issRHFmix <- rbindlist(list(issRHF, issNorge), use.names = TRUE) #bind with correnct coloumnname
issRHFplot <- issRHFmix[iss15==1,]

library(rreg)
fig1 <- regbar(issRHFplot, RHF, pros, comp = "landet", num = tot, ylab = "prosent")

title <- "fig9a_RHFogISS15"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


###################
## fig 9b - HF nivå
issHFNorge <- dataSK[, .N, by=iss15]
issHFNorge[, HF := "Hele landet"]
issHFNorge[, tot := sum(N)]
issHFNorge[, pros := round(N / sum(issNorge$N)*100, digits = 1), by=iss15]


issHF <- dataSK[, .N, by=list(HF, iss15)]
issHF[, tot := sum(N), by=list(HF)]
issHF[, pros := round(N / tot*100, digits = 1), by=.(HF, iss15)]
issHFmix <- rbindlist(list(issHF, issHFNorge), use.names = TRUE) #bind with correnct coloumnname
issHFplot <- issHFmix[iss15==1,]


library(rreg)
fig1 <- regbar(issHFplot, HF, pros, comp = "Hele", num = tot, ylab = "prosent")

title <- "fig9b_HFogISS15"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL



#############################################################################################################
## Fig 10a - NISS > 15 hele landet og per HF, RHF og sykehus

## Landet
nissNorge <- dataSK[, .N, by=niss15]
nissNorge[, RHF := "Hele landet"]
nissNorge[, tot := sum(N)]
nissNorge[, pros := round(N / sum(N, na.rm = T)*100, digits = 1)]

## RHF
nissRHF <- dataSK[, .N, by=list(RHF, niss15)]
nissRHF[, tot := sum(N), by=list(RHF)]
nissRHF[, pros := round(N / tot*100, digits = 1), by=.(RHF, niss15)]
nissRHFmix <- rbindlist(list(nissRHF, nissNorge), use.names = TRUE) #bind with correct coloumnname
nissRHFplot <- nissRHFmix[niss15==1,]

library(rreg)
fig1 <- regbar(nissRHFplot, RHF, pros, comp = "landet", num = tot, ylab = "prosent")

title <- "fig10a_RHFogNISS15"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


###################
## fig 10b - HF nivå
nissHFNorge <- dataSK[, .N, by=niss15]
nissHFNorge[, HF := "Hele landet"]
nissHFNorge[, tot := sum(N)]
nissHFNorge[, pros := round(N / sum(N, na.rm = T)*100, digits = 1)]


nissHF <- dataSK[, .N, by=list(HF, niss15)]
nissHF[, tot := sum(N), by=list(HF)]
nissHF[, pros := round(N / tot*100, digits = 1), by=.(HF, niss15)]
nissHFmix <- rbindlist(list(nissHF, nissHFNorge), use.names = TRUE) #bind with correct coloumnname
nissHFplot <- nissHFmix[niss15==1,]


library(rreg)
fig1 <- regbar(nissHFplot, HF, pros, comp = "Hele", num = tot, ylab = "prosent")

title <- "fig10b_HFogNISS15"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


####################################################################
## fig11 - ed_tta NISS

ttaALL <- dataSK[dataAK, on=c(ntrid="ntrid")]

ttaRaw <- ttaALL[, list(ntrid, ed_tta, iss15, niss15, i.HF, i.RHF, i.Hospital)] #RHF og HF fra akutt data

ttaNorge <- ttaRaw[ed_tta==1, .N, by=niss15][
  !is.na(niss15), pros := N/sum(N, na.rm = T)*100][
    , pros := round(pros, digits = 1)][
      , i.HF:="Hele landet"][
        !is.na(niss15), tot := sum(N, na.rm = T)][niss15==1,]

ttaHF <- ttaRaw[ed_tta==1, .N, by=list(i.HF, niss15)][
  !is.na(niss15), pros := N/sum(N)*100, by=list(i.HF)][
    , pros := round(pros, digits = 1)][
      !is.na(niss15), tot := sum(N, na.rm = T), by=i.HF][niss15==1,]

ttaPlot <- rbindlist(list(ttaHF, ttaNorge), use.names = TRUE)

library(rreg)
fig1 <- regbar(ttaPlot, i.HF, pros, comp = "Hele", num = tot, ylab = "prosent")

title <- "fig11_HF_edtta_NISS_over15"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL

####################################################################
## fig12 - ed_tta ISS

ttaNorgeISS <- ttaRaw[ed_tta==1, .N, by=iss15][
  !is.na(iss15), pros := N/sum(N, na.rm = T)*100][
    , pros := round(pros, digits = 1)][
      , i.HF:="Hele landet"][
        !is.na(iss15), tot := sum(N, na.rm = T)][iss15==1,]

ttaHFISS <- ttaRaw[ed_tta==1, .N, by=list(i.HF, iss15)][
  !is.na(iss15), pros := N/sum(N)*100, by=list(i.HF)][
    , pros := round(pros, digits = 1)][
      !is.na(iss15), tot := sum(N, na.rm = T), by=i.HF][iss15==1,]

ttaPlotISS <- rbindlist(list(ttaHFISS, ttaNorgeISS), use.names = TRUE)

library(rreg)
fig1 <- regbar(ttaPlotISS, i.HF, pros, comp = "Hele", num = tot, ylab = "prosent")

title <- "fig12_HF_edtta_iss_over15"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL

#######################################################################
## fig13 - Skadeintensjon - inj_intention

injInd <- c(1,2,3,99,999)
injNavn <- c("Ulykke", "Selvpåført", "Overfall", "Annen intensjon", "Ukjent")

##2017
injSum <- dataUL[inj_intention!=0, .N, by=inj_intention]
injSum[, pros := round(N /sum(N, na.rm = T)*100, digits = 1)]
##2016
injSum16 <- dataUL16[, .N, by=inj_intention]
injSum16[, pros := round(N /sum(N, na.rm = T)*100, digits = 1)]

injMix <- injSum[injSum16, on=c(inj_intention ="inj_intention")]
injMix[, navn := factor(inj_intention,
                        levels = injInd,
                        labels = injNavn)]


# plotting
## bruk funksjon rapbar
#########################

fig1 <- rapbar(injMix, navn, N, i.N, pros, i.pros, line2 = FALSE, lpos=0.9, lgap=9) #lgap er avstand mellom tallet 2016 og 2017
title <- "fig13_inj_intention"

fig1a <- fig1
grid.draw(fig1a)
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 5, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 5, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 5, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


###########################################################
## fig 14 - pre_transport

preInd <- c(1:6, 88)
preNavn <- c(
"Bilambulanse",
"Ambulansehelikopter",
"Ambulansefly",
"Fraktet inn av publikum",
"Til sykehus selv",
"Politi",
"Annet/Ukjent"
)

preRaw <- dataPH[, list(trans=pre_transport, HF, RHF, Hospital)]
preRaw[, transkat:=trans][trans %in% c(99,999), transkat := 88]

preRaw16 <- dataPH16[, list(trans=pre_transport, HF, RHF, Hospital)]
preRaw16[, transkat:=trans][trans %in% c(99,999), transkat := 88]


##2017
preSum <- preRaw[, .N, by=transkat]
preSum[, pros := round(N/sum(N, na.rm = T)*100, digits = 1)]

##2016
preSum16 <- preRaw16[, .N, by=transkat]
preSum16[, pros := round(N/sum(N, na.rm = T)*100, digits = 1)]

preMix <- preSum[preSum16, on=c(transkat="transkat")]
preMix[, navn := factor(transkat,
                        levels=preInd,
                        labels=preNavn)]


# plotting
## bruk funksjon rapbar
#########################

fig1 <- rapbar(preMix, navn, N, i.N, pros, i.pros, line2 = FALSE, lpos=0.92, lgap=9) #lgap er avstand mellom tallet 2016 og 2017
title <- "fig14_pre_transport"

fig1a <- fig1
grid.draw(fig1a)
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


############################################################################
## fig15 - pre_trans og RHF og HF

preHF <- preRaw[, ]




#########################################################
## fig18- xray_chst

xray <- dataAK[, .N, by=HF]

xNorge <- dataAK[, .N, by=xray_chst]
xNorge[, HF := "Hele landet"]
xNorge[, tot := sum(N)]
xNorge[, pros := round(N / sum(xNorge$N)*100, digits = 1), by=xray_chst]

xHF <- dataAK[, .N, by=list(HF, xray_chst)]
xHF[, tot := sum(N, na.rm = T), by =HF]
xHF[, pros := round(N/tot*100, digits=1), by=list(HF, xray_chst)]
xMix <- rbindlist(list(xHF, xNorge), use.names=T)
xPlot <- xMix[xray_chst==1,]

library(rreg)
fig1 <- regbar(xPlot, HF, pros, comp = "Hele", num = tot, ylab = "prosent")

title <- "fig18_xray_chst"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


#########################################################
## fig19- xray_pelv

xpNorge <- dataAK[, .N, by=xray_pelv]
xpNorge[, HF := "Hele landet"]
xpNorge[, tot := sum(N)]
xpNorge[, pros := round(N / sum(N, na.rm = T)*100, digits = 1)]

xpHF <- dataAK[, .N, by=list(HF, xray_pelv)]
xpHF[, tot := sum(N, na.rm = T), by =HF]
xpHF[, pros := round(N/tot*100, digits=1), by=list(HF, xray_pelv)]
xpMix <- rbindlist(list(xpHF, xpNorge), use.names=T)
xpPlot <- xpMix[xray_pelv==1,]

library(rreg)
fig1 <- regbar(xpPlot, HF, pros, comp = "Hele", num = tot, ylab = "prosent")

title <- "fig19_xray_pelv"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


#########################################################
## fig20- ed_ct

ctNorge <- dataAK[, .N, by=ed_ct]
ctNorge[, HF := "Hele landet"]
ctNorge[, tot := sum(N)]
ctNorge[, pros := round(N / sum(N, na.rm = T)*100, digits = 1)]

ctHF <- dataAK[, .N, by=list(HF, ed_ct)]
ctHF[, tot := sum(N, na.rm = T), by =HF]
ctHF[, pros := round(N/tot*100, digits=1), by=list(HF, ed_ct)]
ctMix <- rbindlist(list(ctHF, ctNorge), use.names=T)
ctPlot <- ctMix[ed_ct==1,]

library(rreg)
fig1 <- regbar(ctPlot, HF, pros, comp = "Hele", num = tot, ylab = "prosent")

title <- "fig20_ed_CT"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL



#########################################################
## fig22 - Res survival
##### HF ########

svNorge <- dataIN[, .N, by=res_survival]
svNorge[, HF := "Hele landet"]
svNorge[, tot := sum(N)]
svNorge[, pros := round(N / sum(N, na.rm = T)*100, digits = 1)]

svHF <- dataIN[, .N, by=list(HF, res_survival)]
svHF[, tot := sum(N, na.rm = T), by =HF]
svHF[, pros := round(N/tot*100, digits=1), by=list(HF, res_survival)]
svMix <- rbindlist(list(svHF, svNorge), use.names=T)
svPlot <- svMix[res_survival==1,]

library(rreg)
fig1 <- regbar(svPlot, HF, pros, comp = "Hele", num = tot, ylab = "prosent")

title <- "fig22_Survival_res_survival"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL

#########################################################
## fig22a - Res survival og ISS>5
#### HF ####
svISS <- dataSK[, list(ntrid, iss15)]
dataSv <- dataIN[svISS, on=c(ntrid="ntrid")]


svNorge2 <- dataSv[iss15==1, .N, by=res_survival]
svNorge2[, HF := "Hele landet"]
svNorge2[, tot := sum(N)]
svNorge2[, pros := round(N / sum(N, na.rm = T)*100, digits = 1)]

svHF2 <- dataSv[iss15==1, .N, by=list(HF, res_survival)]
svHF2[, tot := sum(N, na.rm = T), by =HF]
svHF2[, pros := round(N/tot*100, digits=1), by=list(HF, res_survival)]
svMix2 <- rbindlist(list(svHF2, svNorge2), use.names=T)
svPlot2 <- svMix2[res_survival==1,]

library(rreg)
fig1 <- regbar(svPlot2, HF, pros, comp = "Hele", num = tot, ylab = "prosent")

title <- "fig22a_res_survival_ISS_over15"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL

#################
##### RHF #######
svRNorge <- dataIN[, .N, by=res_survival]
svRNorge[, RHF := "Hele landet"]
svRNorge[, tot := sum(N)]
svRNorge[, pros := round(N / sum(N, na.rm = T)*100, digits = 1)]

svRHF <- dataIN[, .N, by=list(RHF, res_survival)]
svRHF[, tot := sum(N, na.rm = T), by =RHF]
svRHF[, pros := round(N/tot*100, digits=1), by=list(RHF, res_survival)]
svRMix <- rbindlist(list(svRHF, svRNorge), use.names=T)
svRPlot <- svRMix[res_survival==1,]

library(rreg)
fig1 <- regbar(svRPlot, RHF, pros, comp = "Hele", num = tot, ylab = "prosent")

title <- "fig22_Survival_res_survival_regional"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL

#########################################################
## fig22a - Res survival og ISS>5
#### RHF ####

# Sammenligne institusion for forflytting pasinter
insIN <- dataIN[, list(ntrid, RHF, HF, Hospital)]
insSV <- dataSK[, list(ntrid, RHF, HF, Hospital)]
insMix <- insIN[insSV, on=c(ntrid="ntrid")]
insMix[, `:=` (xHF = ifelse(HF!=i.HF, 1L, 0L),
               xRHF = ifelse(RHF != i.RHF, 1L, 0L),
               xHosp = ifelse(Hospital!=i.Hospital, 1L, 0L)), by=ntrid]


svISS <- dataSK[, list(ntrid, iss15)]
dataRSv <- dataIN[svISS, on=c(ntrid="ntrid")]


svRNorge2 <- dataRSv[iss15==1, .N, by=res_survival]
svRNorge2[, RHF := "Hele landet"]
svRNorge2[, tot := sum(N)]
svRNorge2[, pros := round(N / sum(N, na.rm = T)*100, digits = 1)]

svRRHF2 <- dataRSv[iss15==1, .N, by=list(RHF, res_survival)]
svRRHF2[, tot := sum(N, na.rm = T), by =RHF]
svRRHF2[, pros := round(N/tot*100, digits=1), by=list(RHF, res_survival)]
svRMix2 <- rbindlist(list(svRRHF2, svRNorge2), use.names=T)
svRPlot2 <- svRMix2[res_survival==1,]

library(rreg)
fig1 <- regbar(svRPlot2, RHF, pros, comp = "Hele", num = tot, ylab = "prosent")

title <- "fig22a_res_survival_ISS_over15_regional"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL

#########################################################
## fig22a - Res survival og ISS<5
#### RHF ####

svRNorge3 <- dataRSv[iss15==0, .N, by=res_survival]
svRNorge3[, RHF := "Hele landet"]
svRNorge3[, tot := sum(N)]
svRNorge3[, pros := round(N / sum(N, na.rm = T)*100, digits = 1)]

svRRHF3 <- dataRSv[iss15==0, .N, by=list(RHF, res_survival)]
svRRHF3[, tot := sum(N, na.rm = T), by =RHF]
svRRHF3[, pros := round(N/tot*100, digits=1), by=list(RHF, res_survival)]
svRMix3 <- rbindlist(list(svRRHF3, svRNorge3), use.names=T)
svRPlot3 <- svRMix3[res_survival==1,]

library(rreg)
fig1 <- regbar(svRPlot3, RHF, pros, comp = "Hele", num = tot, ylab = "prosent")

title <- "fig22b_res_survival_ISS_under15_regional"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


###############################################################
## Tid til AMK

amkRHF <- dataPH[, list(ntrid, dt_alarm_scene, pre_alarm_dtg, pre_scene_dtg, RHF, HF, Hospital)]
amkRHF[, tid := as.POSIXct(dt_alarm_scene, format="%H:%M:%S", units="mins")]
missAMK <- amkRHF[is.na(tid), list(dt_alarm_scene, pre_alarm_dtg, pre_scene_dtg)]





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
