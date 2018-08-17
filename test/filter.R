## Lager master skjema til filtering
baseFile <- traume[, c("pt_id_ntr",
                       "SkjemaGUID",
                       "PatientAge",
                       "PatientGender",
                       "inj_start_date")]
sykehusFile <- akutt[, c("HovedskjemaGUID", "ed_arrival_dtg")]

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

#####################
## Dygraphs figure ##
#####################

## Time - series
masterFile[, datoAlle := as.POSIXct(dateAll, format = "%d.%m.%Y %H:%M:%S")]

masterFile[, `:=` (datoSykehus = as.POSIXct(dateAll, format = "%d.%m.%Y %H:%M:%S"),
                   datoAlle = as.POSIXct(dateAll, format = "%d.%m.%Y %H:%M:%S"))]


alle <- masterFile[!is.na(datoAlle), .N, by = .(datoAlle)]

library(xts)
timeAlle <- xts(alle$N, order.by = alle$datoAlle)

## install.packages("dygraphs")
library(dygraphs)
maxDato <- strftime(max(alle$datoAlle), format = "%Y-%m-%d")

## Finne dato et år tidligere fra maxDato
library(zoo)
minDato <- strftime(as.Date(as.yearmon(as.Date(maxDato)) - 1, frac = 1))

## dygraph
dygraph(timeAlle,, main = "Antall traume") %>%
  dyRangeSelector(dateWindow = c(minDato, maxDato))


testDate <- strftime(maxDato, format = "%Y-%m-%d")

