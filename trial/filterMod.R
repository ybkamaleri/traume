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

## source("~/Git-work/traume/ntrApp/data.R")

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
        htmlOutput(ns("txtList"))
      )
    ),
    fluidRow(
      verbatimTextOutput(ns("test"))
    ),
    fluidRow(
      verbatimTextOutput(ns("test2"))
    ),
    fluidRow(
    )
  )
}


filterSV <- function(input, output, session, data){

  ns <- session$ns

  navnLevel <- reactive({
    switch(as.character(input$valgLevel01),
      '2' = unique(data$RHF),
      '3' = unique(data$HF),
      '4' = unique(data$Hospital))
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

  ##################
  ###### TEST ######
  ##################

  output$test <- renderPrint({
    input$tidsrom_in[1]
  })

output$test2 <- renderPrint({
  input$alder_in[1]

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
  callModule(filterSV, "filter", resh)

  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
