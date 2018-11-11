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
  "rmarkdown",
  "kableExtra",
  "DT")

sapply(pakke, require, character.only = TRUE)

source("~/Git-work/traume/traumeApp/setup20181110.R")
source("~/Git-work/traume/traumeApp/functions/byttNA.R") #bNA()

## =====
## UI
## =====

rapportUI <- function(id){
  ns <- NS(id)

  fluidPage(
    fluidRow(
      box(width = 3,
        height = 100,
        solidHeader = TRUE,
        status = 'primary',
        title = 'Valg sykehus',
        uiOutput(ns('hosp'))
      ),

      box(width = 6,
        height = 100,
        title = 'Rapport',
        fluidRow(
          column(width = 8,
            radioButtons(ns('format'), 'Fil format',
              choices = c('PDF', 'Word'))
          ),
          column(width = 4,
            downloadButton(ns('rapport_Btn'), label = 'Last ned')
          )
        )
      )
    )
  )

}


## ========
## Server
## =======

rapportSV <- function(input, output, session, resh, data){

  ## ns <- session$ns

  hospValg <- sort(as.factor(unique(resh$Hospital)))
  hospRan <- sample(1:length(hospValg), 1)

  output$hosp <- renderUI({
    selectInput(session$ns("hosp_valg"),
      label = NULL,
      choices = hospValg,
      selected = hospValg[hospRan]
    )
  })


  hospitalNavn <- reactive({input$hosp_valg})

  output$rapport_Btm <- downloadHandler(
    filename = function(){
      paste('rapport', sep = '.',
        switch(input$format, PDF = 'pdf', Word = 'docx')
      )
    },

    content = function(file){
      src <- normalizePath('rapport.Rmd')
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'rapport.Rmd', overwrite = TRUE)

      library(rmarkdown)
      out <- render('rapport.Rmd',
        params = list(rapportTitle = hospitalNavn, rapportDato = Sys.Date()),
        switch(input$format,
          PDF = pdf_document(), Word = word_document())
      )

      file.copy(out, file)
    }
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
  callModule(rapportSV, "rapport", resh, akutt)

  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
