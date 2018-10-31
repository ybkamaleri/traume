## Analyse for konferanse i Nov 2018
rm(list = ls())
pkg <- c("data.table", "ggplot2", "directlabels", "cowplot", "gridExtra", "grid")

sapply(pkg, require, character.only = TRUE)

source("~/Git-work/traume/traumeApp/data2.R")

source("~/Git-work/traume/rapport/annualrap/2018/rapbar.R")
source("conbar.R")
savefig <- "~/Temp/plot"

## Function for tabell
contabel <- function(data, var, select, include, by, pros.digit = 1){
  library(data.table)
  DT <- copy(data)

  all <- DT[get(var) %in% include, .(N = .N), by = by]
  sub <- DT[get(var) == select, .(n = .N), by = by]
  mix <- merge(sub, all, by = by, all.y = TRUE)
  ##rekkefølgen for kolonne n og N må samsvær med når data er merge dvs. som i mix
  mixTot <- rbindlist(list(mix, list("Hele landet", sum(mix$n, na.rm = TRUE), sum(mix$N, na.rm = TRUE))))

  ## bytt NA til 0
  for (j in seq_len(ncol(mixTot))) set(mixTot, which(is.na(mixTot[[j]])), j, 0)
  ## ## bytt <6 til 0
  ## for (j in seq_len(ncol(mixTot))) set(mixTot, which(mixTot[[j]] < 6), j, 0)

  mixTot[, pros := round(n / N * 100, digits = pros.digit), by = by]

  mixTot[, ylab := as.character(pros), by = by]
  mixTot[n < 6, `:=` (ylab = "n<6", pros = 0), by = by]

}


## Data for akutt for bare 2017
akutt17 <- akutt2[dateAll >= as.POSIXct("2017-01-01", format = "%Y-%m-%d") &
                    dateAll <= as.POSIXct("2017-12-31", format = "%Y-%m-%d"),]

## Bort med duplikate og valg bare hosp_serial_num == !
cleanDT <- akutt17[hosp_serial_num == 1 & !duplicated(ntrid), ]


## Akuttmottaksskjema og traumeskjema.
## Røntgen thorax «xray_chst =1» utført ved første sykehus «hosp_serial_num = 1».
## Andel fordelt på HF /sykehus

## Antall all relevante per HF ie. Ukjent og ikke valgt tas bort
sp1a <- contabel(cleanDT, "xray_chst", 1, 1:2, "HF")
fig1 <- conbar(sp1a, HF, pros, ynum = ylab, "Hele", num = N, ylab = "prosent")


fig1 <- rreg::regbar(sp1a, HF, pros, num = n, comp = "Hele", ylab = "prosent")
title <- "rontgen_HF"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


### Hospital
## Antall all relevante per HF ie. Ukjent og ikke valgt tas bort
sp1b <- contabel(cleanDT, "xray_chst", 1, 1:2, "Hospital")
fig1 <- conbar(sp1b, Hospital, pros, ylab, "Hele", num = N, ylab = "prosent")

title <- "Rontgen_sykehus"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL

## Akuttmottaksskjema og traumeskjema.  Røntgen thorax «xray_chst =1» utført ved
## første sykehus «hosp_serial_num = 1», barn «Patient_Age <6 år» sammenlignet med de
## som ikke har fått rtg thorax«xray_chst =2». Andel fordelt på HF /sykehus
data6 <- cleanDT[age < 6, ]
sp2a <- contabel(data6, "xray_chst", 1, 1:2, "RHF")

fig1 <- conbar(sp2a, RHF, pros, ylab, "Hele", num = N, ylab = "prosent", title = "Røntgen thorax for barn under 6 år")

title <- "Rontgen_6aar"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL



#####################################
################ TEST ###############
#####################################

all <- akutt17[hosp_serial_num == 1 & !duplicated(ntrid) & xray_chst %in% 1:2, .(N = .N), by = HF]
## antall utvalgte per HF
valg <- akutt17[hosp_serial_num == 1 & !duplicated(ntrid) & xray_chst == 1, .(n = .N), by = HF]

mix <- valg[all, on = c(HF = "HF")]
mixTotal <- rbindlist(list(mix, list("Hele landet", sum(mix$n), sum(mix$N)))) #tallet for hele landet
mixTotal[, pros := round(n / N * 100, digits = 1), by = HF]
mixTotal

## Antall all relevante per Sykehus ie. Ukjent og ikke valgt tas bort
allhos <- akutt17[hosp_serial_num == 1 & !duplicated(ntrid) & xray_chst %in% 1:2, .(N = .N), by = Hospital]
## antall utvalgte per HF
valghos <- akutt2[hosp_serial_num == 1 & !duplicated(ntrid) & xray_chst == 1, .(n = .N), by = Hospital]

mixhos <- valghos[allhos, on = c(Hospital = "Hospital")]
mixTotalhos <- rbindlist(list(mixhos, list("Hele landet", sum(mixhos$n), sum(mixhos$N)))) #tallet for hele landet
mixTotalhos[, pros := round(n / N * 100, digits = 1), by = Hospital]
mixTotalhos
