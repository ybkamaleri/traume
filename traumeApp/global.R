### pakker ###
pakke <- c("shiny",
  "shinyBS",
  "shinydashboard",
  "data.table",
  "ggplot2",
  "plotly",
  "dygraphs",
  "knitr",
  "kableExtra",
  "xts",
  "zoo",
  "DT")

sapply(pakke, require, character.only = TRUE)

source("data2.R")

#Funksjoner
############
invisible(sapply(list.files('functions', full.names = TRUE), source))

## modules
###########
invisible(sapply(list.files('modules', full.names = TRUE), source))
