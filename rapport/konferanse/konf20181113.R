## Analyse for konferanse i Nov 2018

pkg <- c("data.table", "ggplot2", "directlabels", "cowplot", "gridExtra", "grid")

sapply(pkg, require, character.only = TRUE)

source("/home/ybk/Git-work/traume/traumeApp/data2.R")

source("/home/ybk/Git-work/traume/rapport/annualrap/2018/rapbar.R")
savefig <- "~/Temp/plot"

## Akuttmottaksskjema og traumeskjema.
## Røntgen thorax «xray_chst =1» utført ved første sykehus «hosp_serial_num = 1».
## Andel fordelt på HF /sykehus
dtHosp <- copy(akutt2)

## Antall all relevante per HF ie. Ukjent og ikke valgt tas bort
all <- dtHosp[hosp_serial_num == 1 & !duplicated(ntrid) & xray_chst %in% 1:2, .(N = .N), by = HF]
## antall utvalgte per HF
valg <- akutt2[hosp_serial_num == 1 & !duplicated(ntrid) & xray_chst == 1, .(n = .N), by = HF]

mix <- valg[all, on = c(HF = "HF")]
mixTotal <- rbindlist(list(mix, list("Hele landet", sum(mix$n), sum(mix$N)))) #tallet for hele landet
mixTotal[, pros := round(n / N * 100, digits = 1), by = HF]
mixTotal

fig1 <- rreg::regbar(mixTotal, HF, pros, num = n, comp = "Hele", ylab = "prosent")
title <- "rontgen_HF"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


### Hospital
## Antall all relevante per Sykehus ie. Ukjent og ikke valgt tas bort
allhos <- dtHosp[hosp_serial_num == 1 & !duplicated(ntrid) & xray_chst %in% 1:2, .(N = .N), by = Hospital]
## antall utvalgte per HF
valghos <- akutt2[hosp_serial_num == 1 & !duplicated(ntrid) & xray_chst == 1, .(n = .N), by = Hospital]

mixhos <- valghos[allhos, on = c(Hospital = "Hospital")]
mixTotalhos <- rbindlist(list(mixhos, list("Hele landet", sum(mixhos$n), sum(mixhos$N)))) #tallet for hele landet
mixTotalhos[, pros := round(n / N * 100, digits = 1), by = Hospital]
mixTotalhos

fig1 <- rreg::regbar(mixTotalhos, Hospital, pros, num = n, comp = "Hele", ylab = "prosent")

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

fig1 <- akutt2[hosp_serial_num == 1 & !duplicated(ntrid) & age < 6, .N]
