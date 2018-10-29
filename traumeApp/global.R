### pakker ###
pakke <- c("shiny",
  "shinyBS",
  "shinydashboard",
  "data.table",
  "ggplot2",
  "plotly",
  "dygraphs",
  "xts",
  "zoo",
  "DT")

sapply(pakke, require, character.only = TRUE)

source("data2.R")

#Funksjoner
############
invisible(sapply(list.files('functions', full.names = TRUE), source))
## source("./functions/byttNA.R") #bNA
## source("./functions/plotAgeSex.R") #fun.plotAS


## modules
###########
invisible(sapply(list.files('modules', full.names = TRUE), source))
## source("./modules/filterMod.R")
## source("./modules/virskomhetMod.R")
## source("./modules/ulykkeMod.R")
## source("./modules/skadeMod.R")
