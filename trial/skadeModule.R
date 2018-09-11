
## test ulykke module
#####################

library(shiny)
library(shinydashboard)
library(data.table)
library(ggplot2)
library(shinyBS)

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
      ## Deler for Ryggsøyle
      conditionalPanel(condition = paste0("input['", ns("kropp"), "'] == 6"),
                       selectInput(inputId = ns("til_rygg"),
                                   label = "Spesifisert deler:",
                                   choices = list("Alle" = 1,
                                                  "Cervicalcolumna" = 2,
                                                  "Lumbalcolumna" = 3,
                                                  "Thoracalcolumna" = 4)
                                   )),
      ## Tillegg for Cervicalcolumna
      conditionalPanel(condition = paste0("input['", ns("til_rygg"),
                                          "'] == 2 && input['", ns("kropp"), "'] == 6"),
                       selectInput(inputId = ns("til_cerv"),
                                   label = "Tilleggsuttrekk (nakke):",
                                   choices = list("Alle" = 1,
                                                  "Isolerte skjelettskader" = 2,
                                                  "Ryggmargsskade" = 3
                                                  ),
                                   selected = 1
                                   )),
      ## Tillegg for Lumbalcolumna
      conditionalPanel(condition = paste0("input['", ns("til_rygg"),
                                          "'] == 3 && input['", ns("kropp"), "'] == 6"),
                       selectInput(inputId = ns("til_lumb"),
                                   label = "Tilleggsuttrekk (korsrygg):",
                                   choices = list("Alle" = 1,
                                                  "Isolerte skjelettskader" = 2,
                                                  "Ryggmargsskade" = 3
                                                  ),
                                   selected = 1
                                   )),
      ## Tillegg for Thoracalcolumna
      conditionalPanel(condition = paste0("input['", ns("til_rygg"),
                                          "'] == 4 && input['", ns("kropp"), "'] == 6"),
                       selectInput(inputId = ns("til_thor"),
                                   label = "Tilleggsuttrekk (thorax):",
                                   choices = list("Alle" = 1,
                                                  "Isolerte skjelettskader" = 2,
                                                  "Ryggmargsskade" = 3
                                                  ),
                                   selected = 1
                                   ))
      ),

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
                        value = TRUE),
          bsTooltip(id = ns("skadegrad1"),
                    title = "Hvis skadegrad 1 er valgt må denne også velges",
                    placement = "bottom",
                    trigger = "focus",
                    options = list(container = "body")
                    )
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

  valKropp <- reactive({as.numeric(input$kropp)}) #kroppregion
  valSkade <- reactive({as.numeric(input$skadegrad)}) #skadegrad


  ## Kroppsregioner
  valgKropp  <- reactive({

    ## Alle kroppdeler
    if (as.numeric(input$kropp) == 10) {
      req(input$skadegrad) #vises ingen hvis NULL

      data <- dataIN[, list(n = ifelse(
        sum(grepl(paste0(".[", paste(valSkade(), collapse = ""), "]$"),
                  as.character(unlist(strsplit(ais, split = ","))))) != 0, 1, 0),
        gender = gender), by = ntrid]

    } else {

      data <- dataIN[, list(n = ifelse(
        sum(grepl(paste0("^", valKropp(), ".*[", paste(valSkade(), collapse = ""), "]$"),
                  as.character(unlist(strsplit(ais, split = ","))))) != 0, 1, 0),
        gender = gender), by = ntrid]
    }
    return(data)
  })

  ## Tilleggsuttrekk Abdomen
  tilAbdomen <- reactive({

    ## Milt og lever først 4 tall
    milt <- 5442
    lever <- 5418

    kodeTillegg <- ifelse(as.numeric(input$til_abdomen) == 2, lever, milt)

    if (as.numeric(input$til_abdomen) == 1){

      data <- valgKropp()

    } else {

      data <- dataIN[, list(n = ifelse(
        sum(grepl(paste0("^", kodeTillegg, ".*[", paste(valSkade(), collapse = ""), "]$"),
                  as.character(unlist(strsplit(ais, split = ","))))) != 0, 1, 0),
        gender = gender), by = ntrid]
    }

    return(data)
  })

  ## ## Spine deler
  ## tilSpine <- reactive({

  ##   if (as.numeric(input$kropp) == 6 && as.numeric(input$til_rygg) == 1){
  ##     data <- valgKropp()
  ##   } else if (as.numeric(input$kropp) == 6 && as.numeric(input$til_rygg) == 2){

  ##     ## Cervicalcolumna
  ##     data <-

  ##       } else if (as.numeric(input$kropp) == 6 && as.numeric(input$til_rygg) == 3){

  ##     ## Lumbalcolumna

  ##   } else {

  ##     ## Thoracalcolumna

  ##   }


  ## })


  ## hvis tillegg !=1 så velge output fra tillegg input
  ## ellers velger valgKropp

  ###########
  ## Andel ##
  ###########

  ## Inkluderer Grad 1 eller ikke til å beregne andel
  andelGradAlle  <- reactive({
    if (input$skadegrad1){
      andelG <- dataIN[, list(n = ifelse(
        sum(grepl(".*[1-6]$", as.character(unlist(
          strsplit(ais, split = ","))))) != 0, 1, 0)), by = ntrid]
    } else {
      andelG <- dataIN[, list(n = ifelse(
        sum(grepl(".*[2-6]$", as.character(unlist(
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

    ## progress indikator
    progress <- Progress$new(session, min=1, max=5)
    on.exit(progress$close())

    progress$set(message = 'Vent',
                 detail = 'kalkulering pågår...')

    for (i in 1:5) {
      progress$set(value = i)
      Sys.sleep(0.3)
    }


    data <- valgKropp()[n == 1, .N, by = gender]
    data
  })


  ##################
  ###### TEST ######
  ##################

  output$test <- renderPrint({

    tilAbdomen()[, .N, by = n]
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
