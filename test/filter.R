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


## Time - series
masterFile[, datoAlle := as.Date(dateAll, format = "%d.%m.%Y %H:%M:%S")]

masterFile[, `:=` (datoSykehus = as.Date(dateAll, format = "%d.%m.%Y %H:%M:%S"),
                   datoAlle = as.Date(dateAll, format = "%d.%m.%Y %H:%M:%S"))]


alle <- masterFile[!is.na(datoAlle), .N, by = .(datoAlle)]

library(xts)
timeAlle <- xts(alle$N, order.by = alle$datoAlle)

## install.packages("dygraphs")
library(dygraphs)
dygraph(timeAlle) %>% dyRangeSelector()
