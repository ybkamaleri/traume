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
  "tinytex",
  "zoo",
  "DT")

sapply(pakke, require, character.only = TRUE)

load("./data/ntrDummy.RData")

#Funksjoner
############
invisible(sapply(list.files('functions', full.names = TRUE), source))

## modules
###########
invisible(sapply(list.files('modules', full.names = TRUE), function(x) source(x, encoding = "UTF-8")))
