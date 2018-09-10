## test ulykke module
#####################

library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)

## source("~/Git-work/traume/ntrApp/data.R")

## Module UI
#################
skadeModUI <- function(id){

  ns  <- NS(id)

  fluidPage(
    fluidRow(
      box(with = 4,
          title = "Kroppsregioner",
          selectInput(inputId = ns("kropp"),
                  label = NULL,
                  choices = list("Alle" = 10,
                                 "Head" = 1,
                                 "Face" = 2,
                                 "Neck" = 3,
                                 "Thorax" = 4,
                                 "Abdomen" = 5,
                                 "Spine" = 6,
                                 "Upper extremity" = 7,
                                 "Lower extremity" = 8,
                                 "External and other" = 9
                                 ),
                  selected = 10
                  ),
      ## Tillegg abdomen
      conditionalPanel(condition = paste0("input['", ns("kropp"), "'] == 5"),
                       selectInput(inputId = ns("til_abdomen"),
                                   label = "Tilleggsuttrekk:",
                                   choices = list("Alle" = 1,
                                                  "Leverskader" = 2,
                                                  "Miltskader" = 3
                                                  ),
                                   selected = 3
                                   )),
      ## Tillegg Ryggsøyle
      conditionalPanel(condition = paste0("input['", ns("kropp"), "'] == 6"),
                       selectInput(inputId = ns("til_rygg"),
                                   label = "Tilleggsuttrekk:",
                                   choices = list("Alle" = 1,
                                                  "Cervicalcolumna" = 2,
                                                  "Lumbalcolumna" = 3,
                                                  "Thoracalcolumna" = 4)
                                   ))),
      box(with = 4,
          title = "Skadegradering",
          status = "primary",
          checkboxGroupInput(inputId = ns("skadegrad"),
                             label = NULL,
                             choices = list("2" = 2,
                                            "3" = 3,
                                            "4" = 4,
                                            "5" = 5,
                                            "6" = 6),
                             inline = TRUE,
                             selected = NULL
                             ),
          checkboxInput(inputId = ns("skadegrad1"),
                        label = "Inkluderer skadegrad 1",
                        value = TRUE)
          )
    ),
    fluidRow(
      verbatimTextOutput(ns("test"))
    ),
    fluidRow(
      verbatimTextOutput(ns("test2"))
    )
  )
}


skadeMod <- function(input, output, session, dataFiltert, data){
  dataIN <- data

  dataUT <- reactive({
    ## Alle kroppdeler
    if (as.numeric(input$kropp) == 10) {
      dataIN[, list(n = ifelse(
        sum(grepl(".*[1-6]$", as.numeric(unlist(
          strsplit(ais, split = ","))))) != 0, 1, 0)), by = ntrid]
    } else {
      dataIN[, list(n = ifelse(
        sum(grepl(paste0(".*[", as.numeric(input$kropp), "]$"), as.numeric(unlist(
          strsplit(ais, split = ","))))) != 0, 1, 0)), by = ntrid]
    }
  })


  ## Reactive value
  vars <- reactiveValues()

  ## Velger alle kroppsregioner eller en region
  observe({
    vars$velge <- dataUT()
  })


  ##################
  ###### TEST ######
  ##################

  output$test <- renderPrint({

    vars$velge
  })

  output$test2 <- renderPrint({
    dataUT()[, .N, by = n]

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
        skadeModUI("skade")
      ))
  )
)


server <- function(input, output, session){

  callModule(skadeMod, "skade", data = skade)

  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
