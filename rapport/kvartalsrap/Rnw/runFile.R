## laste opp data
###################
rm(list = ls())


sykehusNavn <- "Drammen"
datoFra <- "2011-01-01"
datoTil <- "2018-12-31"



source("~/Git-work/traume/rapport/kvartalsrap/Rnw/extra/global.R", chdir = TRUE)
source("~/Git-work/traume/rapport/kvartalsrap/Rnw/extra/setup20181110.R", chdir = TRUE)
knitr::knit2pdf("~/Git-work/traume/rapport/kvartalsrap/Rnw/ntrkvartal.Rnw", encoding = "UTF-8",
  output = paste0(sykehusNavn, '.tex'))



## Lage pdf dokumenter for alle med en gang
for (hosp in unique(resh$Hospital)){
  knitr::knit2pdf("~/Git-work/traume/rapport/kvartalsrap/Rnw/all/ntrkvartalAlle.Rnw", encoding = "UTF-8",
    output = paste0(hosp, '.tex'))
}
