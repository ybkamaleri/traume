
## DATA
##========

pakke <- c("shiny",
  "shinyBS",
  "shinydashboard",
  "data.table",
  "ggplot2",
  "dygraphs",
  "xts",
  "zoo",
  "knitr",
  "DT")

sapply(pakke, require, character.only = TRUE)

source("~/Git-work/traume/ntrApp/data2.R")

## ====
## UI
## ====

rapportUI <- function(id){

  ns <- NS(id)

  fluidPage(
    fluidRow(
      box(width = 4,
        uiOutput(ns("hosp"))
      ),

      box(width = 4,
        dateRangeInput(inputId = ns("dato"),
          label = NULL,
          start = Sys.Date() - 360,
          end = Sys.Date(),
          separator = " til ",
          format = "dd.mm.yyyy",
          startview = "month",
          language = "no",
          weekstart = 1)
      ),

      box(width = 4,
        downloadButton(ns("rapport"), label = "Last ned")
      )
    )
  )

}

## =======
## Server
## =======

rapportSV <- function(input, output, session, resh, data){

  ns <- session$ns

  hospValg <- sort(as.factor(unique(resh$Hospital)))
  hospRan <- sample(1:length(hospValg), 1)

  output$hosp <- renderUI({
    selectInput(ns("hosp_valg"),
      label = NULL,
      choices = hospValg,
      selected = hospValg[hospRan]
    )
  })

  output$rapport <- downloadHandler(
    filename = 'ntrrapport.pdf',

    content = function(file) {
      out = knit2pdf('NTRrapport.Rnw', clean = TRUE)
      file.copy(out, file)
    },

    contentType = 'application/pdf'
  )
}



###################################
########## Shiny App ##############
###################################

ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidPage(
      fluidRow(
        rapportUI("rapport")
      ))
  )
)


server <- function(input, output, session){
  callModule(rapportSV, "rapport", resh, akutt2)

  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
