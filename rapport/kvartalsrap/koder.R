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

source("~/Git-work/traume/ntrApp/data2.R")
source("~/Git-work/traume/traumeApp/functions/byttNA.R") #bNA()

## ===============
## Data valg
## ===============

valgSyk <- "Drammen"
datoFra <- "2017-01-01"
datoTil <- "2017-12-31"

dataDT <- akutt2[!duplicated(ntrid) &
                   Hospital == (valgSyk) &
                    dateSykehus >= as.POSIXct(datoFra, format = "%Y-%m-%d") &
                       dateSykehus <= as.POSIXct(datoTil, format = "%Y-%m-%d"), ]



## Antall traume

dataDT[!duplicated(ntrid), .N]


## Antall traume, alder og kjønn
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
