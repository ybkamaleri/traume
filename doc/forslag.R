## Forslag til analyser
#######################################
## Antall hver aldersgruppe per kjønn
#######################################

demo <- traume[!is.na(PatientAge) | PatientAge != -1, .N, by = list(PatientAge, PatientGender)]

library(ggplot2)
ggplot(demo, aes(as.factor(PatientAge), N, group = PatientGender)) +
  geom_line(aes(color = as.factor(PatientGender)))
