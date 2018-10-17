## Årsrapport 2018
rm(list = ls())

pkg <- c("data.table", "rreg", "ggplot2", "directlabels", "cowplot", "gridExtra", "grid")

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

## sted å lage figurer
savefig <- "~/Temp/plot"

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
bNA(ageMix)

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


#### Plotting
#################
data <- accMix[order(accMix$prosent, decreasing = FALSE),] #order prosent
dfrow <- nrow(data) + 1 #for dummy row column ref
data[, ref := as.factor(seq.int(nrow(accMix)))]
data <- rbindlist(list(data, data[NA])) #create dummy row for header table
data[is.na(ref), ref := as.character(dfrow)]

## Gir riktig navn til ulykke typer
data[, navn:= factor(var, levels=navnUT, labels = navnIN)]

## text til tabell
data[, text1:= paste0(n, ";(", prosent, ")"), by=var]
data[, text2:= paste0(i.n, ";(", i.prosent, ")"), by=var]

## Bytt NA til "" for navn, text1 og text2
byttVar <- c("navn", "text1", "text2")
data[ref == "9", `:=` (navn = "",
                       text1 = "",
                       text2 = "")]


## finne høyste y for plassering av tabell
yvar <- data[, list(v1 = max(prosent, na.rm = TRUE), v2 = max(i.prosent, na.rm = TRUE))]
ymax <- ifelse(with(yvar, v1 > v2), yvar$v1, yvar$v2)

## Top text position
yText1 <- ymax + ymax * 0.1
yText2 <- yText1 + 6

## Other paramenters
fsize <- 3 #fontsize

## barplot

Theme001 <- theme(
  axis.text = element_text(size = 10), #text for y and x axis
  axis.ticks.y = element_blank(),
  axis.line = element_blank(),
  axis.title.y = element_blank(), #no title in y axis of plot
  axis.title.x = element_text(margin = margin(t = 10), size = 10),
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid.minor.x = element_blank(),
  ## ## legend.position = "bottom",
  ## legend.justification = c(0,1), #legend left
  ## legend.position =  c(0,1), #legend top
  ## ## legend.box = "horizontal",
  legend.direction = "horizontal",
  legend.title = element_blank(),
  ## legend.key = element_rect(fill = "white"),
  legend.key.width = unit(1, 'lines'), #width key
  legend.spacing.x = unit(0.3, 'cm'), #avstand mellom keys
  legend.text = element_text(size = 9),
  plot.title = element_text(size = 14),
  plot.margin = unit(c(0, 1, 1, 1), 'cm')
)

figUlykke <- ggplot(data) +
  geom_bar(aes(ref, i.prosent, fill =  "2016", color = "2016"), stat = "identity") +
  ## linje mot tallene på tabell
  geom_segment(aes(x = ref, y = 0, xend = ref, yend = ymax), linetype = 2, color = "grey70") +
  ## Dekker top linje
  geom_segment(data = data[ref == as.character(dfrow),],
               aes(x = ref, y = 0, xend = ref, yend = ymax), size = 1, color = "white") +
  geom_segment(aes(x = ref, y = 0, xend = ref, yend = prosent, color = "2017"),
               lineend = "butt", size = 8) +
  scale_fill_manual(values = c("2016" = col1), guide = FALSE) + #for bar
  scale_color_manual(values = c("2016" = col1, "2017" = col3)) + #for segment
  scale_x_discrete(breaks = factor(data$ref), labels = data$navn) +
  labs(y = "prosent") +
  coord_flip() +
  Theme001 +
  ## limit y - axis scale
  scale_y_continuous(expand = c(0,0), breaks = seq(0, ymax, 10)) +
  geom_segment(aes(y = 0, yend = ymax, x = -Inf, xend = -Inf)) +
  guides(color = guide_legend(override.aes = list(fill = "black"))) +
  ## tabell
  geom_text(aes(ref, yText1, label = gsub(";", "\n", text2)), hjust = 0.5, size = fsize) +
  geom_text(aes(ref, yText2, label = gsub(";", "\n", text1)), hjust = 0.5, size = fsize) +
  annotate("text", x = as.character(dfrow), y = yText1,
           label = "2016 \n N (%)", fontface = "bold", size = fsize) +
  annotate("text", x = as.character(dfrow), y = yText2,
           label = "2017 \n N (%)", fontface = "bold", size = fsize)


(figUlykkeAlt <- ggplot(data) +
   ## linje mot tallene på tabell
   geom_segment(aes(x = ref, y = 0, xend = ref, yend = ymax), linetype = 2, color = "grey70") +
   ## Dekker top linje
   geom_segment(data = data[ref == as.character(dfrow),],
                aes(x = ref, y = 0, xend = ref, yend = ymax), size = 1, color = "white") +
   geom_bar(aes(ref, i.prosent, fill = "2016"), stat = "identity") +
   geom_bar(aes(ref, prosent, fill = "2017"), stat = "identity", width = 0.35) +
   scale_fill_manual(values = c("2016" = col1, "2017" = col3)) +
   scale_x_discrete(breaks = factor(data$ref), labels = data$navn) +
   labs(y = "prosent") +
   coord_flip() +
   Theme001 +
   theme(legend.position = c(0,0.92)) + #legend top left
   ## limit y - axis scale
   scale_y_continuous(expand = c(0,0), breaks = seq(0, ymax, 10)) +
   geom_segment(aes(y = 0, yend = ymax, x = -Inf, xend = -Inf)) +
   ## tabell
   geom_text(aes(ref, yText1, label = gsub(";", "\n", text2)), hjust = 0.5, size = fsize) +
   geom_text(aes(ref, yText2, label = gsub(";", "\n", text1)), hjust = 0.5, size = fsize) +
   annotate("text", x = as.character(dfrow), y = yText1,
            label = "2016 \n N (%)", fontface = "bold", size = fsize) +
   annotate("text", x = as.character(dfrow), y = yText2,
            label = "2017 \n N (%)", fontface = "bold", size = fsize)
)

## Thing som endre
## avstand mellom text (yText2)
## legend.position c(0,0.95)



## save file generic
fig1 <- figUlykke
title <- "ulykketyper"

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
transNavn <- c("Bil", "MC/Moped", "Sykkel", "Båt", "Tog", "Annet/Ukjent")
transInd <- c(1:5,9)

transMF[, navn := factor(transkode,
                         levels = transInd,
                           labels = transNavn)]


## plotting
transPlot <- transMF[order(transMF$pros, decreasing = FALSE),]
dfrow <- nrow(transPlot) + 1
transPlot[, ref := as.factor(seq.int(nrow(transPlot)))]
transPlot <- rbindlist(list(transPlot, transPlot[NA]))
transPlot[is.na(ref), ref := as.character(dfrow)]

## text til å bruke til plot label
transPlot[, text1 := paste0(N, ";(", pros, "%)"), by = ref]
transPlot[, text2 := paste0(i.N, ";(", i.pros, "%)"), by = ref]

## Bytt NA til "" for navn, text1 og text2
byttVar <- c("navn", "text1", "text2")
transPlot[ref == as.character(dfrow), `:=` (navn = "",
                                            text1 = "",
                                            text2 = "")]
## hvis <5 tar bort !!OBSS!!!
transPlot[N == 1, `:=` (text1 = "n<6")]


## finne høyste y for plassering av tabell
yvar <- transPlot[, list(v1 = max(pros, na.rm = TRUE), v2 = max(i.pros, na.rm = TRUE))]
ymax <- ifelse(with(yvar, v1 > v2), yvar$v1, yvar$v2)

## Top text position
yText1 <- ymax + ymax * 0.1
yText2 <- yText1 + 8

## Other paramenters
fsize <- 3 #fontsize


Theme001 <- theme(
  axis.text = element_text(size = 10), #text for y and x axis
  axis.ticks.y = element_blank(),
  axis.line = element_blank(),
  axis.title.y = element_blank(), #no title in y axis of plot
  axis.title.x = element_text(margin = margin(t = 10), size = 10),
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid.minor.x = element_blank(),
  legend.position = "bottom",
  legend.justification = c(0,1), #legend bottom left
  ## legend.box = "horizontal",
  legend.direction = "horizontal",
  legend.title = element_blank(),
  ## legend.key = element_rect(fill = "white"),
  legend.key.width = unit(1, 'lines'), #width key
  legend.spacing.x = unit(0.3, 'cm'), #avstand mellom keys
  legend.text = element_text(size = 9),
  plot.title = element_text(size = 14),
  plot.margin = unit(c(0, 1, 1, 1), 'cm')
)

figTrans <- ggplot(transPlot) +
  geom_bar(aes(ref, i.pros, fill =  "2016", color = "2016"), stat = "identity") +
  ## linje mot tallene på tabell
  geom_segment(aes(x = ref, y = 0, xend = ref, yend = ymax), linetype = 2, color = "grey70") +
  ## Dekker top linje
  geom_segment(data = transPlot[ref == as.character(dfrow),],
               aes(x = ref, y = 0, xend = ref, yend = ymax), size = 1, color = "white") +
  geom_segment(aes(x = ref, y = 0, xend = ref, yend = pros, color = "2017"),
               lineend = "butt", size = 8) +
  scale_fill_manual(values = c("2016" = col1), guide = FALSE) + #for bar
  scale_color_manual(values = c("2016" = col1, "2017" = col3)) + #for segment
  scale_x_discrete(breaks = factor(transPlot$ref), labels = transPlot$navn) +
  labs(y = "prosent") +
  coord_flip() +
  Theme001 +
  ## limit y - axis scale
  scale_y_continuous(expand = c(0,0), breaks = seq(0, ymax, 10)) +
  geom_segment(aes(y = 0, yend = ymax, x = -Inf, xend = -Inf)) +
  guides(color = guide_legend(override.aes = list(fill = "black"))) +
  ## tabell
  geom_text(aes(ref, yText1, label = gsub(";", "\n", text2)), hjust = 0.5, size = fsize) +
  geom_text(aes(ref, yText2, label = gsub(";", "\n", text1)), hjust = 0.5, size = fsize) +
  annotate("text", x = as.character(dfrow), y = yText1,
           label = "2016 \n N (%)", fontface = "bold", size = fsize) +
  annotate("text", x = as.character(dfrow), y = yText2,
           label = "2017 \n N (%)", fontface = "bold", size = fsize)



## save file generic
fig1 <- figTrans
title <- "transport_type"

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


########################################
## 7. Skademekanisme - inj_mechanism

ulykke[, ]






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
