###########################################
##     Type ulykker og skadegradering    ##
###########################################

## Valgte ais-koder
indAis <- grep("Valgte ais", names(skade)) #finne indeks til kolonne
names(skade)[indAis] <- "ais"  #gir nytt navn til Valgte ais-koder

################################################
## kombinere alle skadekoder fra samme NTR-nr
## og tar bort dublikate koder
###############################################
#ta bort alle missing NTR-nr.
skade <- skade[!is.na(ntrid), ]

## kombinere alle skadekoder og valgt bare unike koder
## fra forskjellige sykehus for hver NTR-nr og variable navn blir "aiskode"
skade[skade[!is.na(ntrid),
            toString(unique(unlist(
              strsplit(ais, split = ",")))), by = ntrid],
      on = "ntrid", aiskode := i.V1]


######################################
## Dele skade koder til flere kolonne
## install.packages("splitstackshape", repos = "https://cran.uib.no", dependencies = TRUE)

library(splitstackshape)
skadeDel <- splitstackshape::cSplit(skade, splitCols = "aiskode", sep = ",", direction = "wide", drop = FALSE)


################################
## Velge ulykke og skadegrader

##################################
## merge ulykke og skade skjemaer
##################################
## ## beholder relevant kolonne fra ulykkeskjema
## accName <- grep("acc_transport", names(ulykke)):grep("acc_fire_inhal", names(ulykke))
## ulykkeSub <- ulykke[, .SD, .SDcols = accName] #valg bare disse kolonner

### Beholder alle var i skadeskjema
### alle var starter med i. kommer fra skade skjema
skadeUlykke <- ulykke[skade, on = "ntrid"]

## henter index fra acc_trans til acc_fire
accName <- grep("acc_transport", names(skadeUlykke)):grep("acc_fire_inhal", names(skadeUlykke))
## convert to numeric
for (d in accName) {
  set(skadeUlykke, j = d, value = as.numeric(skadeUlykke[[d]]))
}

#########################
## legger til HF og RHF
#########################
skadeUlykke[, i.UnitId := as.numeric(i.UnitId)]
resh[, reshid := as.numeric(reshid)]

skadeGrad <- resh[skadeUlykke, on = c(reshid = "i.UnitId")]


#######################
## Valg ulyketype
#######################
acd <- 1
body <- 4
grad1 <- 1
grad2 <- 1
varValg <- "acc_trsp_rd_type"

setkey(skadeGrad, ntrid)

##testGrad <- skadeGrad[!duplicated(ntrid) & !is.na(ntrid), ]

skadeAlvor <- skadeGrad[get(varValg) == acd & !duplicated(ntrid) & !is.na(ntrid),
                        list(ja = ifelse(
                          sum(grepl(paste0("^", body, ".*[", grad1, "-", grad2, "]$"),
                                    as.numeric(unlist(
                                      strsplit(aiskode, split = ","))))) != 0, 1, 0),
                          syk = i.HealthUnitName,
                          hf = HF,
                          rhf = RHF), #bruk i.HealthUnitName som kommer fra skadeskjema
                        by = c("ntrid")]

skadeAlvor[ja == 1, .N, by = syk]
skadeAlvor[ja == 1, .N, by = "hf"]
skadeAlvor[ja == 1, .N, by = "rhf"]

#########################
## Andre ulykke
########################
## 1 - Transportulykke
## 2 - Fallulykke
## 3 - Voldsulykke
## 4 - Arbeidsulykke
## 5 - Sport og fritid
## 6 - Brann og inhalasjonsskade
## 7 - Annen ulykke

accT <- 2
body <- 4
grad1 <- 1
grad2 <- 1

accKode <- switch(accT,
                  "acc_transport",
                  "acc_fall",
                  "acc_violence",
                  "acc_self_inflict",
                  "acc_work",
                  "acc_sprt_recreat",
                  "acc_fire_inhal",
                  "acc_other")

setkey(skadeGrad, ntrid)

##testGrad <- skadeGrad[!duplicated(ntrid) & !is.na(ntrid), ]

skadeAndre <- skadeGrad[get(accKode) == 1 & !duplicated(ntrid) & !is.na(ntrid), list(ja = ifelse(sum(grepl(
  paste0("^", body, ".*[", grad1, "-", grad2, "]$"), as.numeric(unlist(
    strsplit(aiskode, split = ","))))) != 0, 1, 0),
  syk = i.HealthUnitName,
  hf = HF,
  rhf = RHF), #bruk i.HealthUnitName som kommer fra skadeskjema
  by = c("ntrid")]

skadeAndre[ja == 1, .N, by = syk]
skadeAndre[ja == 1, .N, by = c("hf", "rhf")]

## select 1 at first and either 2 or 3 at the end
grepl("^1.*[23]$", test1)


### Sjekke koder i datadump
## OPS!! IKKE konvertere ais til numeric for å beholder koder som starter med 0
skade09 <- skadeGrad[!is.na(ntrid), .(unlist(strsplit(ais, split = ",")))]
head(skade09)
nrow(skade09)

skade099 <- skade09[, .(ais0 = grep("^09", V1, value = T))]
nrow(skade099)

skade0 <- skade09[, .(ais0 = grep("^0", V1, value = T))]
nrow(skade0)

skade9 <- skade09[, .(ais0 = grep("^9", V1, value = T))]
nrow(skade9)


a00 <- data.table(a = c("011", "112"))
grep("^0", a00$a, value = TRUE)
a00[, .(N = grep("^0", a, value = T))]
