### pakker ###

pakke <- c("shiny", "shinydashboard", "data.table", "ggplot2")

sapply(pakke, require, character.only = TRUE)


## ## alternativ
## lapply(pakke, FUN = function(X) {
##   do.call("require", list(X))
## })
