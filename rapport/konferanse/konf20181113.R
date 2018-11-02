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

##=================
## DATA utvalg
##=================

## Data for akutt for bare 2017
akutt17 <- akutt2[dateAll >= as.POSIXct("2017-01-01", format = "%Y-%m-%d") &
                    dateAll <= as.POSIXct("2017-12-31", format = "%Y-%m-%d"),]

## Bort med duplikate og valg bare hosp_serial_num == !
cleanDT <- akutt17[hosp_serial_num == 1 & !duplicated(ntrid), ]

## Skade data for 2017
skade17 <- skade[dateAll >= as.POSIXct("2017-01-01", format = "%Y-%m-%d") &
                   dateAll <= as.POSIXct("2017-12-31", format = "%Y-%m-%d"),]

cleanSkade <- skade17[hosp_serial_num == 1 & !duplicated(ntrid), .(ntrid, UnitId, inj_iss)]

## Skade og akutt data merge
mixData <- cleanDT[cleanSkade, on = c(ntrid = "ntrid")]

## Intensiv data for 2017
int17 <- intensiv[dateAll >= as.POSIXct("2017-01-01", format = "%Y-%m-%d") &
                    dateAll <= as.POSIXct("2017-12-31", format = "%Y-%m-%d"),]

cleanInt <- int17[hosp_serial_num == 1 & !duplicated(ntrid), .(ntrid, res_survival)]


## Prehospital data for 2017
pre17 <- prehosp[dateAll >= as.POSIXct("2017-01-01", format = "%Y-%m-%d") &
                   dateAll <= as.POSIXct("2017-12-31", format = "%Y-%m-%d"),]

cleanPre <- pre17[hosp_serial_num == 1 & !duplicated(ntrid),
  .(ntrid, UnitId, pre_intubated, pre_gcs_sum)]






##================
## Analyse
##================

## Akuttmottaksskjema og traumeskjema.
## Røntgen thorax «xray_chst =1» utført ved første sykehus «hosp_serial_num = 1».
## Andel fordelt på HF /sykehus

## Antall all relevante per HF ie. Ukjent og ikke valgt tas bort
sp1a <- contabel(cleanDT, "xray_chst", 1, 1:2, "HF")
fig1 <- conbar(sp1a, HF, pros, ynum = ylab, "Hele", num = N, ylab = "prosent",
  title = "Røntgen thorax")


fig1 <- rreg::regbar(sp1a, HF, pros, num = n, comp = "Hele", ylab = "prosent")
title <- "rontgen_thor_HF"
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
fig1 <- conbar(sp1b, Hospital, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "Røntgen thorax")

title <- "Rontgen_thor_sykehus"
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

fig1 <- conbar(sp2a, RHF, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "Røntgen thorax for barn under 6 år")

title <- "Rontgen_thor_6aar_RHF"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL

## HF
data6 <- cleanDT[age < 6, ]
sp2b <- contabel(data6, "xray_chst", 1, 1:2, "HF")

fig1 <- conbar(sp2b, HF, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "Røntgen thorax for barn under 6 år")

title <- "Rontgen_thor_6aar_HF"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


##################################
## Spørsmål 3
#################################
## xray_chst = 1, inj_iss > 15, hosp_serial_num = 1


dataISS15 <- mixData[inj_iss > 15, ] #bare for ISS > 15

## HF
sp3a <- contabel(dataISS15, "xray_chst", 1, 1:2, "HF")

fig1 <- conbar(sp3a, HF, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "Røntgen thorax for ISS > 15")

title <- "Rontgen_thor_ISS_HF"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL

## RHF
sp3b <- contabel(dataISS15, "xray_chst", 1, 1:2, "RHF")

fig1 <- conbar(sp3b, RHF, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "Røntgen thorax for ISS > 15")

title <- "Rontgen_thor_ISS_RHF"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL

#######################################
## Spørsmål 4
#######################################
## xray_pelv = 1
## Bruk cleanDT for akutt data subset for 2017 og ikke duplikate

## HF
sp4a <- contabel(cleanDT, "xray_pelv", 1, 1:2, "HF")

fig1 <- conbar(sp4a, HF, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "Røntgen bekken")

title <- "Rontgen_pelv_HF"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


## Sykehus
sp4b <- contabel(cleanDT, "xray_pelv", 1, 1:2, "Hospital")

fig1 <- conbar(sp4b, Hospital, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "Røntgen bekken")

title <- "Rontgen_pelv_Sykehus"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


#########################
## Spørsmål 5
#########################
## Bruk dataISS15 som allerede sortet for ISS >15. Her velges det xray_pelv=1

## HF
sp5a <- contabel(dataISS15, "xray_pelv", 1, 1:2, "HF")

fig1 <- conbar(sp5a, HF, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "Røntgen bekken for ISS > 15")

title <- "Rontgen_pelv_ISS_HF"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


## Sykehus
sp5b <- contabel(dataISS15, "xray_pelv", 1, 1:2, "Hospital")

fig1 <- conbar(sp5b, Hospital, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "Røntgen bekken for ISS > 15")

title <- "Rontgen_pelv_ISS_Sykehus"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


#######################
## Spørsmål 6
#######################
## Bruk cleanDT data for ed_ct=1

## HF
sp6a <- contabel(cleanDT, "ed_ct", 1, 1:2, "HF")

fig1 <- conbar(sp6a, HF, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "Utført CT")

title <- "Utført_CT_HF"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


## Sykehus
sp6b <- contabel(cleanDT, "ed_ct", 1, 1:2, "Hospital")

fig1 <- conbar(sp6b, Hospital, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "Utført CT")

title <- "Utført_CT_sykehus"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


######################################
## Spørsmål 7
######################################
## Utført CT og ISS > 15 - dataISS15

## HF
sp7a <- contabel(dataISS15, "ed_ct", 1, 1:2, "HF")

fig1 <- conbar(sp7a, HF, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "Utført CT og ISS > 15")

title <- "Utført_CT_ISS_HF"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


## Hospital
sp7b <- contabel(dataISS15, "ed_ct", 1, 1:2, "Hospital")

fig1 <- conbar(sp7b, Hospital, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "Utført CT og ISS > 15")

title <- "Utført_CT_ISS_sykehus"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


##################################
## Spørsmål 8
#################################
## bruk mixData for å filtrere ISS < 4

skadeIss4 <- mixData[inj_iss < 4, ]


## HF
sp8a <- contabel(skadeIss4, "ed_ct", 1, 1:2, "HF")

fig1 <- conbar(sp8a, HF, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "Utført CT og ISS < 4")

title <- "Utført_CT_iss4_HF"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


## Hospital
sp8b <- contabel(skadeIss4, "ed_ct", 1, 1:2, "Hospital")

fig1 <- conbar(sp8b, Hospital, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "Utført CT og ISS < 4")

title <- "Utført_CT_iss4_sykehus"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


####################################
## Spørsmål 9a
####################################
## res_survival = 1 og ISS under og over 15
mixSI <- cleanSkade[cleanInt, on = c(ntrid = "ntrid")]
## legge RHF, HF og Hospital
mixSkadInt <- mixSI[resh, on = c(UnitId = "reshid")]

mixSIover15 <- mixSkadInt[inj_iss > 15 & !duplicated(ntrid), ]
mixSIunder15 <- mixSkadInt[inj_iss < 15 & !duplicated(ntrid), ]

## HF ISS > 15
sp9a <- contabel(mixSIover15, "res_survival", 1, 1:2, "HF")

fig1 <- conbar(sp9a, HF, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "30 dagers mortalitet og ISS > 15")

title <- "mortaliet_over15_HF"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


## HF ISS < 15
sp9b <- contabel(mixSIunder15, "res_survival", 1, 1:2, "HF")

fig1 <- conbar(sp9b, HF, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "30 dagers mortalitet og ISS < 15")

title <- "mortaliet_under15_HF"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


#############################
## Spørsmål 9b
#############################
## pre_gcs_sum < 9 og pre_intubated = 2

DTpre <- cleanPre[resh, on = c(UnitId = "reshid")]

## HF
sp9b1 <- contabel(DTpre, "pre_intubated", 2, 1:2, "HF")
fig1 <- conbar(sp9b1, HF, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "Har ikke utført luftveistiltak")

title <- "intubated_HF"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


## Hospital
sp9b2 <- contabel(DTpre, "pre_intubated", 2, 1:2, "Hospital")
fig1 <- conbar(sp9b2, Hospital, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "Har ikke utført luftveistiltak")

title <- "intubated_Hospital"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


####################################
## Spørsmål 9c
####################################

gcs9 <- DTpre[pre_gcs_sum < 9, ]


## HF
sp9c1 <- contabel(gcs9, "pre_intubated", 2, 1:2, "HF")
fig1 <- conbar(sp9c1, HF, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "Har ikke utført luftveistiltak med GCS < 9")

title <- "intubated_gcs_HF"
fig1a <- fig1
cowplot::save_plot(paste0(savefig, "/", title, ".jpg"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".png"), fig1a, base_height = 7, base_width = 7)
cowplot::save_plot(paste0(savefig, "/", title, ".pdf"), fig1a, base_height = 7, base_width = 7)
dev.off()

## reset fig1 - to avoid wrong figure
fig1 <- NULL


## Hospital
sp9c2 <- contabel(gcs9, "pre_intubated", 2, 1:2, "Hospital")
fig1 <- conbar(sp9c2, Hospital, pros, ylab, "Hele", num = N, ylab = "prosent",
  title = "Har ikke utført luftveistiltak med GCS < 9")

title <- "intubated_gcs_Hospital"
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
