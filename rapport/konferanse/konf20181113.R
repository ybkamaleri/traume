## Analyse for konferanse i Nov 2018
source("/home/ybk/Git-work/traume/traumeApp/data2.R")

source("/home/ybk/Git-work/traume/rapport/annualrap/2018/rapbar.R")

## Akuttmottaksskjema og traumeskjema.
## Røntgen thorax «xray_chst =1» utført ved første sykehus «hosp_serial_num = 1».
## Andel fordelt på HF /sykehus

## Antall all relevante per HF ie. Ukjent og ikke valgt tas bort
all <- dtHosp[hosp_serial_num == 1 & !duplicated(ntrid) & xray_chst %in% 1:2, .(N = .N), by = HF]
## antall utvalgte per HF
valg <- akutt2[hosp_serial_num == 1 & !duplicated(ntrid) & xray_chst == 1, .(n = .N), by = HF]

mix <- valg[all, on = c(HF = "HF")]
mixTotal <- rbindlist(list(mix, list("Hele landet", sum(mix$n), sum(mix$N)))) #tallet for hele landet
mixTotal[, pros := round(n / N * 100, digits = 1), by = HF]
mixTotal

rreg::regbar(mixTotal, HF, pros, num = n, comp = "Hele", ylab = "prosent")

### Hospital
## Antall all relevante per Sykehus ie. Ukjent og ikke valgt tas bort
allhos <- dtHosp[hosp_serial_num == 1 & !duplicated(ntrid) & xray_chst %in% 1:2, .(N = .N), by = Hospital]
## antall utvalgte per HF
valghos <- akutt2[hosp_serial_num == 1 & !duplicated(ntrid) & xray_chst == 1, .(n = .N), by = Hospital]

mixhos <- valghos[allhos, on = c(Hospital = "Hospital")]
mixTotalhos <- rbindlist(list(mixhos, list("Hele landet", sum(mixhos$n), sum(mixhos$N)))) #tallet for hele landet
mixTotalhos[, pros := round(n / N * 100, digits = 1), by = Hospital]
mixTotalhos

rreg::regbar(mixTotalhos, Hospital, pros, num = n, comp = "Hele", ylab = "prosent")
