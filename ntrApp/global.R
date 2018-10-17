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
source("./misc/byttna.R") #bNA
source("./misc/functionPlot.R") #plotreg


## modules
###########
source("./modules/virksomhet.R")
source("./modules/virksomhetPlot.R")
source("./modules/ulykkeMod.R")
source("./modules/skadeModule.R")
