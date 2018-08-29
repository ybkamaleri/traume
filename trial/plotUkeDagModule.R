library(shiny)
library(data.table)
library(ggplot2)

## source("~/Git-work/traume/ntrApp/data.R")

inputModUI <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("hosp")),
    dateRangeInput(inputId = ns("virk_date_in"),
                            label = NULL,
                            start = Sys.Date() - 30, #alt. min date
                            end = Sys.Date(),
                            separator = "til",
                            format = "dd-mm-yyyy",
                            startview = "month",
                            language = "no",
                            weekstart = 1))
}

inputMod <- function(input, output, session, resh){

  helseEnhet <- as.factor(unique(resh$Hospital))

  ## list sykehusnavn
  output$hosp <- renderUI({
    ns <- session$ns
    selectInput(ns("hosp_in"),
                label = NULL,
                choices = sort(helseEnhet),
                selected = sort(helseEnhet)[1])
  })


  ## Sykehusnavn
  return(list(value = reactive({input$hosp_in}),
              datoFra = reactive({input$virk_date_in[1]}),
              datoTil = reactive({input$virk_date_in[2]})))

}


outputModUI <- function(id){
  ns <- NS(id)
  tagList(
    verbatimTextOutput(ns("txt")),
    plotOutput(ns("ggplot"))
  )
}


outputMod <- function(input, output, session, valg){
  hospNavn <-quote(valg$value())
  datoFra <- quote(valg$datoFra())
  datoTil <- quote(valg$datoTil())

  output$txt <- renderPrint({
    paste0("Test: ", eval(hospNavn))
  })

  ## output$txt <- renderPrint({
  ##   paste0("Test: ", valg$value())
  ## })

  output$ggplot <- renderPlot({

    valgDato <- akutt2[!duplicated(ntrid) & !is.na(dateSykehus) &
                         Hospital == eval(hospNavn) &
                           dateSykehus >= as.Date(eval(datoFra), format = "%Y-%m-%d") &
                          dateSykehus <= as.Date(eval(datoTil), format = "%Y-%m-%d")]
    valgDag <- valgDato[, dag := weekdays(dateSykehus)]
    ntot <- dim(valgDag)[1] #total dager
    ukeDag <- valgDag[, .(pros = round((.N / ntot) * 100),
                          n = .N), by = dag]


    ukeDag$dag <- factor(ukeDag$dag, levels = c("mandag", "tirsdag", "onsdag", "torsdag",
                                                "fredag", "lørdag", "søndag"))

    ggplot(ukeDag, aes(dag, pros)) + geom_bar(stat = "identity")
  })
}



ui <- fluidPage(
  inputModUI("virk"),
  outputModUI("plot")
)

server <- function(input, output, session){
  myValg <- callModule(inputMod, "virk", resh)
  callModule(outputMod, "plot", myValg)

  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
