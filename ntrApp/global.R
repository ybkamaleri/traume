### pakker ###

pakke <- c("shiny",
           "shinyBS",
           "shinydashboard",
           "data.table",
           "ggplot2",
           "dygraphs",
           "xts",
           "zoo",
           "DT")

sapply(pakke, require, character.only = TRUE)

## ## alternativ
## lapply(pakke, FUN = function(X) {
##   do.call("require", list(X))
## })

source("data.R")

#Funksjoner
############

## bytt NA med 0
bNA <- function(DT, na = 0){
  for (j in seq_len(ncol(DT)))
    set(DT,which(is.na(DT[[j]])),j, na)
}
