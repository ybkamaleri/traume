
#####################
## Dygraphs figure ##
#####################
rm(list = ls())
source("/home/yuskam/Git-work/traume/ntrApp/data.R")

## ## Time - series
## masterFile[, datoAlle := as.POSIXct(dateAll, format = "%d.%m.%Y %H:%M:%S")]

## masterFile[, `:=` (datoSykehus = as.POSIXct(dateAll, format = "%d.%m.%Y %H:%M:%S"),
##                    datoAlle = as.POSIXct(dateAll, format = "%d.%m.%Y %H:%M:%S"))]


dateAll_dt <- masterFile[!is.na(dateAll), .N, by = .(dateAll)]
dateMann_dt <- masterFile[!is.na(dateAll)][gender == 1,.N, by = .(dateAll)]
dateKvinne_dt <- masterFile[!is.na(dateAll)][gender == 2,.N, by = .(dateAll)]

tsTraumeSub <- dateMann_dt[dateAll_dt, on = c(dateAll = "dateAll")]
tsTraumeAll <- dateKvinne_dt[tsTraumeSub, on = c(dateAll = "dateAll")]


## replace all NA to 0
## tsTraumeAll[is.na(tsTraumeAll)] <- 0


## replace NA to 0 with function by number - RASKERE!
rep0 <- function(DT){
  for (j in seq_len(ncol(DT)))
    set(DT, which(is.na(DT[[j]])),j,0)
}

rep0(tsTraumeAll)

## library(microbenchmark)
## microbenchmark(
##   rep = rep0(tsTraumeAll),
##   na = tsTraumeAll[is.na(tsTraumeAll)] <- 0
## )


library(xts)
tsAlle <- xts::xts(tsTraumeAll$i.N.1, order.by = tsTraumeAll$dateAll)
tsMann <- xts::xts(tsTraumeAll$i.N, order.by = tsTraumeAll$dateAll)
tsKvinne <- xts::xts(tsTraumeAll$N, order.by = tsTraumeAll$dateAll)

timeTraumeAlle <- cbind(tsAlle, tsMann, tsKvinne)

## install.packages("dygraphs")
library(dygraphs)
dygraph(timeTraumeAlle, main = "Antall Traume per dag og kjønn") %>%
  dySeries("..1", label = "Alle") %>%
  dySeries("..2", label = "Menn") %>%
  dySeries("..3", label = "Kvinner") %>%
  dyHighlight(highlightCircleSize = 5,
              highlightSeriesBackgroundAlpha = 0.3,
              hideOnMouseOut = FALSE) %>%
    dyRangeSelector()

## MaxDato for range selector
maxDato <- strftime(as.POSIXct(max(alle$datoAlle)),format = "%d.%m.%Y %H:%M:%S")

## Finne dato et år tidligere fra maxDato
library(zoo)
minDato <- strftime(as.POSIXct(zoo::as.yearmon(as.POSIXct(maxDato, format = "%d.%m.%Y %H:%M:%S")) - 1, frac = 1))

## dygraph
dygraph(timeAlle,, main = "Antall traume") %>%
  dyRangeSelector(dateWindow = c(minDato, maxDato))



###########################
## ggplot2
##########################
library(ggplot2)

ggAll <- ggplot(tsTraumeAll, aes(x = dateAll)) +
  geom_line(aes(y = N, text = paste0("Kvinner"))) +
  geom_line(aes(y = i.N, color = "Menn")) +
  geom_line(aes(y = i.N.1, color = "Alle")) +
  ylab("Antall") +
  theme_minimal() +
  scale_color_manual(name = NULL,
                     values = c(Menn = "blue", Kvinner = "lightblue", Alle = "orange"))


library(plotly)
ggplotly(ggAll)


## lagt long data
gName <- c("Kvinner","Menn","Alle")
setnames(tsTraumeAll, 2:4, gName)

tsTraumeAll_long <- melt(tsTraumeAll, id.vars = "dateAll")

genderLong <- ggplot(tsTraumeAll_long,
                     aes(x = dateAll, y = value,
                         colour = variable)) + geom_line()

ggplotly(genderLong)


######################
## dygraph month year
######################

tsTraumeAll_my <- tsTraumeAll[, list(kvinner = N,
                                     menn = i.N,
                            alle = i.N.1,
                            dateAll = format(dateAll, "%Y-%m"))]
