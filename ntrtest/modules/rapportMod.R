## Module for rapport

## ====
## UI
## ====

rapportUI <- function(id){

  ns <- NS(id)

  fluidPage(
    fluidRow(
      box(width = 4,
        height = 100,
        solidHeader = TRUE,
        status = "primary",
        title = "Valg sykehus:",
        uiOutput(ns("hosp"))
      ),

      box(width = 4,
        height = 100,
        solidHeader = TRUE,
        status = "primary",
        title = "Valg periode:",
        dateRangeInput(inputId = ns("dato_rapport"),
          label = NULL,
          start = Sys.Date() - 2660,
          end = Sys.Date() - 1360,
          separator = " til ",
          format = "dd.mm.yyyy",
          startview = "month",
          language = "no",
          weekstart = 1)
      ),

      box(width = 4,
        height = 100,
        align = "center", offset = 2,
        ## title = "Rapport",
        background = "light-blue",
        tags$div(HTML("<div style='color: #FFFFFF; font-size: 18px; font-weight: bold; text-align: center;'>Trykk knappen for å lage rapporten i PDF</div>")),
        tags$br(),
        downloadButton(ns("rapport"), label = "Last ned", class = "butangDownload")
      )
    ),
    fluidRow(
      includeMarkdown("rapport.md")
    )
  )

}

## =======
## Server
## =======

rapportSV <- function(input, output, session, resh, data){

  library(tinytex)

  ns <- session$ns

  hospValg <- sort(as.factor(unique(resh$Hospital)))
  hospRan <- sample(1:length(hospValg), 1)

  output$hosp <- renderUI({
    selectInput(ns("hosp_rapport"),
      label = NULL,
      choices = hospValg,
      selected = hospValg[hospRan]
    )
  })

  output$rapport <- downloadHandler(
    filename = 'ntrrapport.pdf',

    content = function(file) {
      out = knitr::knit2pdf('doc/NTRrapport.Rnw', clean = TRUE)
      file.rename(from = out, to = file)
    },

    contentType = 'application/pdf'
  )
}
