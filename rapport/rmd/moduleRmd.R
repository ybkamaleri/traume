modUI <- function(id){

  ns <- NS(id)

  fluidPage(
  title = 'Download a PDF report',
  sidebarLayout(
    sidebarPanel(
      helpText(),
      selectInput(ns('x'), 'Build a regression model of mpg against:',
                  choices = names(mtcars)[-1]),
      radioButtons(ns('format'), 'Document format', c('PDF', 'HTML', 'Word'),
                   inline = TRUE),
      downloadButton(ns('downloadReport'))
    ),
    mainPanel(
      plotOutput(ns('regPlot'))
    )
  )
)

}

modServer <- function(input, output, session){

  regFormula <- reactive({
    as.formula(paste('mpg ~', input$x))
  })

  output$regPlot <- renderPlot({
    par(mar = c(4, 4, .1, .1))
    plot(regFormula(), data = mtcars, pch = 19)
  })

  valgtit <- "Sendiri punya"

  output$downloadReport <- downloadHandler(
    filename = function() {
      paste('my-report', sep = '.', switch(
        input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
      ))
    },

    content = function(file) {
      src <- normalizePath('report.Rmd')

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'report.Rmd', overwrite = TRUE)

      library(rmarkdown)
      out <- render('report.Rmd',
        params = list(dynamictitle = valgtit, reportdate = Sys.Date() - 360),
        switch(input$format,
        PDF = pdf_document(), HTML = html_document(), Word = word_document()
      ))
      file.copy(out, file)
    }
  )
}




## APP

library(shiny)
ui <- fluidPage(
  fluidRow(
    modUI("testMD")
  )
)

server <- function(input, output, session){
  callModule(modServer, "testMD")

  session$onSessionEnded(stopApp)
}

shinyApp(ui = ui, server = server)
