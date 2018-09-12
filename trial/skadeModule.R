
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
                                       selected = 1),
                           bsTooltip(id = ns("til_cerv"),
                                     title = "Telling for minst en av de allerede spesifiserte AIS koder",
                                     placement = "right",
                                     trigger = "hover",
                                     options = list(container = "body"))),
          ## Tillegg for Lumbalcolumna
          conditionalPanel(condition = paste0("input['", ns("til_rygg"),
                                              "'] == 3 && input['", ns("kropp"), "'] == 6"),
                           selectInput(inputId = ns("til_lumb"),
                                       label = "Tilleggsuttrekk (korsrygg):",
                                       choices = list("Alle" = 1,
                                                      "Isolerte skjelettskader" = 2,
                                                      "Ryggmargsskade" = 3
                                                      ),
                                       selected = 1),
                           bsTooltip(id = ns("til_lumb"),
                                     title = "Telling for minst en av de allerede spesifiserte AIS koder",
                                     placement = "right",
                                     trigger = "hover",
                                     options = list(container = "body"))),
          ## Tillegg for Thoracalcolumna
          conditionalPanel(condition = paste0("input['", ns("til_rygg"),
                                              "'] == 4 && input['", ns("kropp"), "'] == 6"),
                           selectInput(inputId = ns("til_thor"),
                                       label = "Tilleggsuttrekk (thorax):",
                                       choices = list("Alle" = 1,
                                                      "Isolerte skjelettskader" = 2,
                                                      "Ryggmargsskade" = 3
                                                      ),
                                       selected = 1),
                           bsTooltip(id = ns("til_thor"),
                                     title = "Telling for minst en av de allerede spesifiserte AIS koder",
                                     placement = "right",
                                     trigger = "hover",
                                     options = list(container = "body")))),

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

  ## OBS!! bruk 'aisMis' for å velge skade gradering
  dataRaw <- data
  dataRaw[, aisMix := toString(unlist(strsplit(ais, split = ","))), by = ntrid]


  dataIN <- dataRaw[!duplicated(ntrid)]
  dataIN[, ais := NULL] #slett ais siden aisMix inneholder alle ais koder

  valKropp <- reactive({as.numeric(input$kropp)}) #kroppregion
  valSkade <- reactive({as.numeric(input$skadegrad)}) #skadegrad


  ## Kroppsregioner
  #########################
  ## Minst en av valgte deler + skadegradering

  valgKropp  <- reactive({

    ## Alle kroppdeler
    if (as.numeric(input$kropp) == 10) {
      req(input$skadegrad) #vises ingen hvis NULL

      data <- dataIN[, list(n = ifelse(
        sum(grepl(paste0(".[", paste(valSkade(), collapse = ""), "]$"),
                  as.character(toString(unlist(strsplit(aisMix, split = ",")))))) != 0, 1, 0),
        gender = gender,
        aisMix = aisMix), by = ntrid]

    } else {

      data <- dataIN[, list(n = ifelse(
        sum(grepl(paste0("^", valKropp(), ".*[", paste(valSkade(), collapse = ""), "]$"),
                  as.character(toString(unlist(strsplit(aisMix, split = ",")))))) != 0, 1, 0),
        gender = gender,
        aisMix = aisMix), by = ntrid]
    }
    return(data)
  })

  ## Tilleggsuttrekk Abdomen
  ##########################
  ## Minst en av valgte deler + skadegradering

  tilAbdomen <- reactive({

    req(input$skadegrad) #vises ingen hvis NULL

    ## Milt og lever først 4 tall
    milt <- 5442
    lever <- 5418

    kodeTillegg <- ifelse(as.numeric(input$til_abdomen) == 2, lever, milt)

    if (as.numeric(input$til_abdomen) == 1){

      data <- valgKropp()

    } else {

      data <- valgKropp()[, list(n = ifelse(
        sum(grepl(paste0("^", kodeTillegg, ".*[", paste(valSkade(), collapse = ""), "]$"),
                  as.character(toString(unlist(strsplit(aisMix, split = ",")))))) != 0, 1, 0),
        gender = gender,
        aisMix = aisMix), by = ntrid]
    }

    return(data)
  })

  ## Spine deler
  #########################
  ## Minst en av valgte deler + skadegradering
  tilSpine <- reactive({

    req(input$skadegrad) #vises ingen hvis NULL

    if (as.numeric(input$kropp) == 6 && as.numeric(input$til_rygg) == 1){
      data <- valgKropp()
    } else if (as.numeric(input$kropp) == 6 && as.numeric(input$til_rygg) == 2){

      ## Cervicalcolumna - femte tallet er 2
      data <- valgKropp()[, list(n = ifelse(
        sum(grepl(paste0("6[0-9][0-9][0-9]2.*[", paste(valSkade(), collapse = ""), "]$"),
                  as.character(toString(unlist(strsplit(aisMix, split = ",")))))) != 0, 1, 0),
        gender = gender,
        aisMix = aisMix), by = ntrid]

    } else if (as.numeric(input$kropp) == 6 && as.numeric(input$til_rygg) == 3){

      ## Lumbalcolumna - femte tallet er 6
      data <- valgKropp()[, list(n = ifelse(
        sum(grepl(paste0("6[0-9][0-9][0-9]6.*[", paste(valSkade(), collapse = ""), "]$"),
                  as.character(toString(unlist(strsplit(aisMix, split = ",")))))) != 0, 1, 0),
        gender = gender,
        aisMix = aisMix), by = ntrid]

    } else {

      ## Thoracalcolumna - femte tallet er 4
      data <- valgKropp()[, list(n = ifelse(
        sum(grepl(paste0("6[0-9][0-9][0-9]4.*[", paste(valSkade(), collapse = ""), "]$"),
                  as.character(toString(unlist(strsplit(aisMix, split = ",")))))) != 0, 1, 0),
        gender = gender,
        aisMix = aisMix), by = ntrid]

    }

    return(data)

  })


  ## Spine Tilleggsuttrekk - Cervicalcolumna
  tilCerv <- eventReactive(input$til_cerv, {

    kode_skjelett <- "^6502[123][024678].*[23]$"
    kode_rygg <- "^6402.*[3456]$"

    ## kode er skjelettskader og kode2 ryggmargsskade
    dataSK <- dataIN[, list(
      kode = ifelse(
        sum(grepl(kode_skjelett,
                  unlist(strsplit(aisMix, split = ",")))) != 0, 1, 0),
      kode2 = ifelse(
        sum(grepl(kode_rygg,
                  unlist(strsplit(aisMix, split = ",")))) != 0, 1, 0),
      ntrid = ntrid,
      gender = gender), by = ntrid]

    #kode1 0 hvis begge skjelettskader og ryggmargsskade
    dataSK[, kode1 := kode, by = ntrid]
    dataSK[, kode1 := ifelse(kode == 1 && kode2 == 1, 0, kode), by = ntrid]

    if (as.numeric(input$til_cerv) == 1){
      data <- tilSpine()
    } else if (as.numeric(input$til_cerv) == 2){
      data <- dataSK[kode1 == 1, list(n = 1, gender = gender), by = ntrid]
    } else {
      data <- dataSK[kode2 == 1, list(n = 1, gender = gender), by = ntrid]
    }

    return(data)
  })


  ## Spine tilleggsuttrekk - Lumbalcolumna
  tilLumb <- eventReactive(input$til_lumb, {

    kode_skjelett <- "^6506[123][024678].*[23]$"
    kode_rygg <- "^6406.*[345]$"

    ## kode er skjelettskader og kode2 ryggmargsskade
    dataSK <- dataIN[, list(
      kode = ifelse(
        sum(grepl(kode_skjelett,
                  unlist(strsplit(aisMix, split = ",")))) != 0, 1, 0),
      kode2 = ifelse(
        sum(grepl(kode_rygg,
                  unlist(strsplit(aisMix, split = ",")))) != 0, 1, 0),
      ntrid = ntrid,
      gender = gender), by = ntrid]
    
    #kode1 0 hvis begge skjelettskader og ryggmargsskade
    dataSK[, kode1 := kode, by = ntrid]
    dataSK[, kode1 := ifelse(kode == 1 && kode2 == 1, 0, kode), by = ntrid]
    
    if (as.numeric(input$til_lumb) == 1){
      data <- tilSpine()
    } else if (as.numeric(input$til_lumb) == 2){
      data <- dataSK[kode1 == 1, list(n = 1, gender = gender), by = ntrid]
    } else {
      data <- dataSK[kode2 == 1, list(n = 1, gender = gender), by = ntrid]
    }
    
    return(data)

  })


  ## Spine tilleggsuttrekk - Thoracalcolumna
  tilThor <- eventReactive(input$til_thor, {

    kode_skjelett <- "^6504[123][024678].*[23]$"
    kode_rygg <- "^6404.*[345]$"
    
    ## kode er skjelettskader og kode2 ryggmargsskade
    dataSK <- dataIN[, list(
      kode = ifelse(
        sum(grepl(kode_skjelett,
                  unlist(strsplit(aisMix, split = ",")))) != 0, 1, 0),
      kode2 = ifelse(
        sum(grepl(kode_rygg,
                  unlist(strsplit(aisMix, split = ",")))) != 0, 1, 0),
      ntrid = ntrid,
      gender = gender), by = ntrid]
    
    #kode1 0 hvis begge skjelettskader og ryggmargsskade
    dataSK[, kode1 := kode, by = ntrid]
    dataSK[, kode1 := ifelse(kode == 1 && kode2 == 1, 0, kode), by = ntrid]
    
    if (as.numeric(input$til_thor) == 1){
      data <- tilSpine()
    } else if (as.numeric(input$til_thor) == 2){
      data <- dataSK[kode1 == 1, list(n = 1, gender = gender), by = ntrid]
    } else {
      data <- dataSK[kode2 == 1, list(n = 1, gender = gender), by = ntrid]
    }
    
    return(data)

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
        sum(grepl(".*[1-6]$", as.character(unlist(
          strsplit(aisMix, split = ","))))) != 0, 1, 0)), by = ntrid]
    } else {
      andelG <- dataIN[, list(n = ifelse(
        sum(grepl(".*[2-6]$", as.character(unlist(
          strsplit(aisMix, split = ","))))) != 0, 1, 0)), by = ntrid]
    }
    andelG[, sum(n, na.rm = TRUE)]
  })

  ## Andell med grad 1 eller ikke for spesifiserte kroppsregion
  andelGradKropp <- reactive({
    if (input$skadegrad1){
      andelG <- dataIN[, list(n = ifelse(
        sum(grepl(paste0("^", valKropp(), ".*[1-6]$"),
                  as.character(unlist(strsplit(aisMix, split = ","))))) != 0, 1, 0),
        gender = gender, aisMix = aisMix), by = ntrid]
    } else {
      andelG <- dataIN[, list(n = ifelse(
        sum(grepl(paste0("^", valKropp(), ".*[2-6]$"),
                  as.character(unlist(strsplit(aisMix, split = ","))))) != 0, 1, 0),
        gender = gender, aisMix = aisMix), by = ntrid]
    }

    data <- andelG[, sum(n, na.rm = TRUE)]
    return(data)
  })

  ############
  ## Tabell ##
  ############
  tabUT <- reactive({

    req(input$skadegrad) #vises ingen hvis NULL

    ## progress indikator
    progress <- Progress$new(session, min=1, max=5)
    on.exit(progress$close())

    progress$set(message = 'Vent',
                 detail = 'kalkulering pågår...')

    for (i in 1:5) {
      progress$set(value = i)
      Sys.sleep(0.3)
    }


    data <- andelGradKropp()
    data
  })


  ##################
  ###### TEST ######
  ##################

  output$test <- renderPrint({

    tabUT()

  })

  output$test2 <- renderPrint({

    andelGradKropp()
    ## data <- andelGradKropp()[]
    ## print(data, topn = 20)
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
