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
          start = Sys.Date() - 360,
          end = Sys.Date(),
          separator = " til ",
          format = "dd.mm.yyyy",
          startview = "month",
          language = "no",
          weekstart = 1)
      ),

      box(width = 4,
        height = 100,
        title = "Rapport",
        downloadButton(ns("rapport"), label = "Last ned")
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
      out = knit2pdf('NTRrapport.Rnw', clean = TRUE)
      file.copy(out, file)
    },

    contentType = 'application/pdf'
  )
}
