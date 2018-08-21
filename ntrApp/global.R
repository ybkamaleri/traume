### pakker ###

pakke <- c("shiny",
           "shinyBS",
           "shinydashboard",
           "data.table",
           "ggplot2",
           "dygraphs",
           "xts",
           "zoo")

sapply(pakke, require, character.only = TRUE)

## ## alternativ
## lapply(pakke, FUN = function(X) {
##   do.call("require", list(X))
## })

source("data.R")
