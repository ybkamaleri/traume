## Lager master skjema til filtering
baseFile <- traume[, c("pt_id_ntr",
                       "SkjemaGUID",
                       "PatientAge",
                       "PatientGender",
                       "inj_start_date")]
sykehusFile <- akutt[, c("HovedskjemaGUID", "ed_arrival_dtg")]

masterFile <- sykehusFile[baseFile, on = c(HovedskjemaGUID = "SkjemaGUID")]

newName <- c("Age", "Gender", "dateAll", "dateSykehus")
setnames(masterFile, c("PatientAge", "PatientGender", "inj_start_date", ))
