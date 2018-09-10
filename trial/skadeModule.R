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
                             choices = list("1" = 1,
                                            "2" = 2,
                                            "3" = 3,
                                            "4" = 4,
                                            "5" = 5,
                                            "6" = 6),
                             inline = TRUE,
                             selected = 1
                             ),
          checkboxInput(inputId = ns("skadegrad1"),
                        label = "Andel inkluderer skadegrad 1",
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

  ## Kroppsregioner
  valgKropp  <- reactive({

    valKropp <- as.numeric(input$kropp) #kroppregion
    valSkade <- as.numeric(input$skadegrad) #skadegrad

    ## Alle kroppdeler
    if (as.numeric(input$kropp) == 10) {
      req(input$skadegrad) #vises ingen hvis NULL

      dataIN[, list(n = ifelse(
        sum(grepl(paste0(".[", paste(valSkade, collapse = ""), "]$"),
                  as.numeric(unlist(strsplit(ais, split = ","))))) != 0, 1, 0),
        gender = gender), by = ntrid]

    } else {

      dataIN[, list(n = ifelse(
        sum(grepl(paste0("^", valKropp, ".*[", paste(valSkade, collapse = ""), "]$"),
                  as.numeric(unlist(strsplit(ais, split = ","))))) != 0, 1, 0),
        gender = gender), by = ntrid]
    }
  })

  ## hvis tillegg !=1 så velge output fra tillegg input
  ## ellers velger valgKropp

  ###########
  ## Andel ##
  ###########

  ## Inkluderer Grad 1 eller ikke til å beregne andel
  andelGradAlle  <- reactive({
    if (input$skadegrad1){
      andelG <- dataIN[, list(n = ifelse(
        sum(grepl(".*[1-6]$", as.numeric(unlist(
          strsplit(ais, split = ","))))) != 0, 1, 0)), by = ntrid]
    } else {
      andelG <- dataIN[, list(n = ifelse(
        sum(grepl(".*[2-6]$", as.numeric(unlist(
          strsplit(ais, split = ","))))) != 0, 1, 0)), by = ntrid]
    }
    andelG[, sum(n, na.rm = TRUE)]
  })

  ## Reactive value
  vars <- reactiveValues()

  ## Velger alle kroppsregioner eller en region
  observe({
    vars$velge <- andelGradAlle()
  })


  ############
  ## Tabell ##
  ############
  tabUT <- reactive({
    data <- valgKropp()[n == 1, .N, by = gender]
    data
  })


  ##################
  ###### TEST ######
  ##################

  output$test <- renderPrint({

    andelGradAlle()
  })

  output$test2 <- renderPrint({

    tabUT()

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
