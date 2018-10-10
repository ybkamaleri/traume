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

source("data2.R")

#Funksjoner
############
source("./functions/byttNA.R") #bNA
source("./functions/plotAgeSex.R") #fun.plotAS


## modules
###########
source("./modules/filterMod.R")
source("./modules/virskomhetMod.R")
source("./modules/ulykkeMod.R")
source("./modules/skadeMod.R")
