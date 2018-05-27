## Forslag til analyser
#######################################
## Antall hver aldersgruppe per kjønn
#######################################
traume[PatientAge == -1, PatientAge := NA] #bytt -1 til NA
## finner ut hvordan velger man -1 f.eks PatientAge!=-1 ikke funker. Koden nedenfor eksluderer ikke -1
## demo <- traume[PatientAge > -1 | !is.na(PatientAge) , .N, keyby = list(PatientGender, PatientAge)]

demo <- traume[!is.na(PatientAge) , .N, keyby = list(PatientGender, PatientAge)]

## Summen for hver alderskategori
demo[, Sum := sum(N), by = list(PatientAge)]

library(ggplot2)
ggplot(demo, aes(as.factor(PatientAge), N, group = PatientGender, color = as.factor(PatientGender))) +
  geom_line() + theme_linedraw()

ggplot(demo) +
  geom_line(aes(as.factor(PatientAge), N, group = PatientGender, color = as.factor(PatientGender))) +
  geom_line(aes(as.factor(PatientAge), Sum))
