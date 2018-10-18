## pakker
#####################

pakke <- c("shiny",
  "shinyBS",
  "shinydashboard",
  "data.table",
  "ggplot2",
  "dygraphs",
  "xts",
  "zoo",
  "DT")

sapply(pakke, require, character.only = TRUE)
## library(shiny)
## library(shinydashboard)
## library(data.table)
## library(ggplot2)

## source("~/Git-work/traume/ntrApp/data2.R")

## Module UI
#################
filterUI <- function(id){

  ns  <- NS(id)

  fluidPage(
    fluidRow(
      box(width = 3, height = 165,
        selectInput(ns("valgLevel01"), "Analysenivå:",
          choices = c("Hele landet" = 1,
            "RHF" = 2,
            "HF" = 3,
            "Sykehus" = 4),
          selected = 1),
        ## updateSelectInput i server for choices oppdateringsliste
        conditionalPanel(condition = 'input.valgLevel01!=1', ns = ns,
          selectInput(ns("valgLevel02"), label = "",
            choices = "",
            selected = ""))),
      box(width = 3, height = 165,
        dateRangeInput(inputId = ns("tidsrom_in"),
          label = "Valg dato fra og til",
          start = Sys.Date() - 175, #alt. min date
          end = Sys.Date(),
          separator = "til",
          format = "dd.mm.yyyy",
          startview = "month",
          language = "no",
          weekstart = 1)),
      box(width = 3, height = 165,
        sliderInput(inputId = ns("alder_in"),
          label = "Valg aldersgruppe",
          min = 0, max = 120,
          value = c(0,110)),
        checkboxInput(inputId = ns("alder_kat"),
          label = "Grupperer alder",
          value = FALSE),
        bsTooltip(id = "alder_in",
          title = "Alle traume ID med missing data for alder blir eksludert",
          placement = "bottom",
          trigger = "focus",
          options = list(container = "body"))),
      box(width = 3, height = 165, background = "light-blue",
        tags$h4("Filtert info:"),
        htmlOutput(ns("txtList")),
        column(width = 10, offset = 4,
          tags$br(),
          actionButton(ns("runButton"), label = "Bruk valget",
            style = 'padding:5px 30px; border: none; text-align: center; font-size:15px;' ))
      )
    ),
    fluidRow(
      verbatimTextOutput(ns("test"))
    ),
    fluidRow(
      verbatimTextOutput(ns("test2"))
    ),
    fluidRow(
      tabBox(side = 'left', selected = "Figur", width = 12,
        tabPanel("Figur", plotOutput(ns("fig"))),
        tabPanel("Tabell", DT::dataTableOutput(ns("tabell"))))
    )
  )
}


filterSV <- function(input, output, session, resh, data){

  ns <- session$ns

  navnLevel <- reactive({
    switch(as.character(input$valgLevel01),
      '2' = unique(resh$RHF),
      '3' = unique(resh$HF),
      '4' = unique(resh$Hospital))
  })

  valgNavn <- reactive({
    switch(as.character(input$valgLevel01),
      '2' = "Valg RHF",
      '3' = "Valg HF",
      '4' = "Valg Sykehus")
  })


  ## oppdaterer liste over RHF, HF eller Sykehus
  observeEvent(input$valgLevel01, {
    updateSelectInput(session, "valgLevel02",
      label = valgNavn(),
      choices = sort(navnLevel()),
      selected = "")
  })

  ## Tekst for utvalg filter
  output$txtList <- renderUI({

    if (input$valgLevel01 == 1){
      valgUnit <-  paste0("Data for hele landet")
    }else{
      valgUnit <- paste0("Data for ", input$valgLevel02)
    }

    valgTid <- paste0("Tidsrom: ",
      format(as.Date(as.character(input$tidsrom_in[1])), "%d.%m.%Y"),
      " til ",
      format(as.Date(input$tidsrom_in[2]), "%d.%m.%Y")
    )

    valgAge <- paste0("Aldersgruppe: ",
      input$alder_in[1],
      " til ",
      input$alder_in[2], " år")

    HTML(paste0(valgUnit, br(), valgTid, br(), valgAge))

  })

  ## Filtert data
  dataFil <- eventReactive(input$runButton, {

    datoFra <- as.POSIXct(input$tidsrom_in[1], format = "%Y-%m-%d")
    datoTil <- as.POSIXct(input$tidsrom_in[2], format = "%Y-%m-%d")

    ageFra <- as.numeric(input$alder_in[1])
    ageTil <- as.numeric(input$alder_in[2])

    ## Tar bort missing ntrid og alder og kjønn
    cleanData <- data[!is.na(ntrid) & !duplicated(ntrid) & !is.na(age) & !is.na(gender)]

    ## Filter dato
    dataDate <- switch(input$valgLevel01,
      '1' = {cleanData[dateAll >= datoFra & dateAll <= datoTil, ]},
      '2' = {cleanData[RHF == input$valgLevel02 & dateAll >= datoFra & dateAll <= datoTil, ]},
      '3' = {cleanData[HF == input$valgLevel02 & dateAll >= datoFra & dateAll <= datoTil, ]},
      '4' = {cleanData[Hospital == input$valgLevel02 & dateAll >= datoFra & dateAll <= datoTil, ]})

    ## Filter alder
    dataSub <- dataDate[age >= ageFra & age <= ageTil, ]

    dataSub
  })

  ## byttNA
  source("/home/yuskam/Git-work/traume/ntrApp/misc/byttna.R")
  #plotreg function
  source("/home/yuskam/Git-work/traume/ntrApp/misc/functionPlot.R")


  dataUT <- eventReactive(input$runButton, {

    if (input$alder_kat){
      valgUT <- plotreg(dataFil(), kat = TRUE)
    } else {
      valgUT <- plotreg(dataFil())
    }
    valgUT
  })

  ## Plot alder og kjønn
  output$fig <- renderPlot({
    plotUT <- dataUT()$plot
    print(plotUT)
  })

  ## Tabell for alder og kjønn
  output$tabell <- DT::renderDT({
    dataUT()$data
  })


  ##################
  ###### TEST ######
  ##################

  output$test <- renderPrint({
    str(input$valgLevel01)
  })

  output$test2 <- renderPrint({
    str(dataFil())

  })

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
        filterUI("filter")
      ))
  )
)


server <- function(input, output, session){
  callModule(filterSV, "filter", resh, masterFile)

  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
