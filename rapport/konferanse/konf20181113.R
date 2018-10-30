## Analyse for konferanse i Nov 2018
source("/home/ybk/Git-work/traume/traumeApp/data2.R")

## Akuttmottaksskjema og traumeskjema.
## Røntgen thorax «xray_chst =1» utført ved første sykehus «hosp_serial_num = 1».
## Andel fordelt på HF /sykehus

dt <- akutt2[!duplicated(ntrid) & xray_chst == 1 & hosp_serial_num == 1, .N, by = HF]

dt
