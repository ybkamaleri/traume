## for å lage plot

plotreg <- function(data, kat = FALSE, bykat = 5){


  ## Rense alder
  dataClean <- data[!is.na(age) & age != -1, .N, keyby = list(age, gender)]

  ## Interval for alder kategorien
  by5 = bykat

  ## Alder kategorisering
  if (kat){
    ## Alder kategori
    alder.kat <- function(x, lower, upper, by, sep = "-") {
      ## Finne høyeste kategori
      kat <- paste0(seq(lower + by - 1, upper - 1, by = by))
      indTop <- max(length(kat))
      top <- as.numeric(kat[indTop])

      labs <- paste0(c(paste(seq(lower, upper - by, by = by),
                             seq(lower + by - 1, upper - 1, by = by),
                             sep = sep),
                       paste(top + 1, "+", sep = "")), " år")
      cut(floor(x), breaks = c(seq(lower, upper, by = by), Inf),
          include.lowest = TRUE, right = FALSE, labels = labs)}

    ageMax <- max(as.numeric(dataClean$age), na.rm = TRUE)
    ageMin <- min(as.numeric(dataClean$age), na.rm = TRUE)

    data <- dataClean[, ageValg := alder.kat(age, ageMin, ageMax, by5)]

  } else {
        data <- dataClean[, ageValg := age]
  }


  ageMan <- dataClean[gender == 1, list(mann = N), by = ageValg]
  ageKvinne <- dataClean[gender == 2, list(kvinne = N), by = ageValg]
  ageMK <- merge(ageMan, ageKvinne, all = TRUE)


}
