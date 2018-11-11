
pakke <- c("shiny",
  "shinyBS",
  "shinydashboard",
  "data.table",
  "ggplot2",
  "dygraphs",
  "xts",
  "zoo",
  "knitr",
  "rmarkdown",
  "DT")

sapply(pakke, require, character.only = TRUE)


rmarkdown::render("~/Git-work/traume/rapport/kvartalsrap/traumeRap.Rmd",
    params=list(dynamictitle="Test title",
                reportdate=Sys.Date()-1)
                )
