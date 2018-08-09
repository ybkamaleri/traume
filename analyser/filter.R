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
