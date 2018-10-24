source("../ntrApp/data.R")

library(ggplot2)

## Sjekke alder range
summary(masterFile$age)
## inneholder NA og -1 som skal bort
masterFile[,.N, by = age]

cleanAgeTraume <- masterFile[!is.na(age) & age != -1,.N, keyby = list(age, gender)]

ageMan <- cleanAgeTraume[gender == 1, list(mann = N), key = age]
ageKvinne <- cleanAgeTraume[gender == 2, list(kvinne = N), key = age]
ageMK <- merge(ageMan, ageKvinne, all = TRUE)


## bytt NA med 0
bNA <- function(DT, na = 0){
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j, na)
}

bNA(ageMK)
## lage summen for alle
ageMK[, alle := mann + kvinne, by = age]

## konverterer data til long
dataLongAK <-melt(ageMK, id.vars="age", measure.vars=c("mann","kvinne","alle"), variable.name="gender", value.name="n")

dataLongAK2 <-melt(ageMK, id.vars="age", measure.vars=c("alle", "mann","kvinne"), variable.name="gender", value.name="n") # rekkefølge for alle, mann, kvinne for legend

## plot with long data
ggplot(dataLongAK, aes(age, n, group = gender, color = gender)) +
  geom_line() +
  geom_point()

ggplot(dataLongAK, aes(age, n, color = gender)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("blue", "lightblue", "orange"))

ggplot(dataLongAK2, aes(age, n, color = gender)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("blue", "lightblue", "orange"))

## plot
dev.new()
library(ggplot2)
plotAgeMK <- ggplot(ageMK, aes(x = age)) +
  geom_line(aes(y = mann, color = "Menn"), size = 1) +
  geom_line(aes(y = kvinne, color = "Kvinner"), size = 1) +
  geom_line(aes(y = alle, color = "Begge"), size = 1) +
  ## title(xlab = "Alder", ylab = "Antall") +
  xlab("Alder") +
  ylab("Antall") +
  theme_minimal() +
  scale_color_manual(name = NULL,
                     values = c(Menn = "blue", Kvinner = "lightblue", Begge = "orange"))
plotAgeMK
