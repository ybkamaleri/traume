## Data
source("data.R")

## merge traume to resh and keep resh
data001 <- traume[resh, on = c(UnitId = "reshid")]

## merge resh to traume and keep traume
data002 <- resh[traume, on = c(reshid = "UnitId")]

data002[, .N, by = .(reshid)]
traume[, .N, by = .(UnitId)]

data002[, .N, by = list(RHF, HF)]
data002[, .N, by = list(RHF, HF, HealthUnitName)]

## Lager master skjema til filtering
baseFile <- traume[, c("pt_id_ntr",
                       "SkjemaGUID",
                       "PatientAge",
                       "PatientGender",
                       "inj_start_date")]
sykehusFile <- akutt[, c("HovedskjemaGUID", "ed_arrival_dtg")]

## Beholder alle i baseFile. Row i akuttfil som ikke har kombling til SkjemaGUID i
## traume slettes
masterFile <- sykehusFile[baseFile, on = c(HovedskjemaGUID = "SkjemaGUID")]

newName <- c("ntrid","Age", "Gender", "dateSykehus", "dateAll")

setnames(masterFile, c("pt_id_ntr",
                       "PatientAge",
                       "PatientGender",
                       "ed_arrival_dtg",
                       "inj_start_date"), newName)

ntrid  <- as.numeric(gsub("^NTR-", "", masterFile$ntrid))

## lik antall
length(unique(ntrid))
length(unique(masterFile$ntrid))
