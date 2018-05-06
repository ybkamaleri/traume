
library(shiny)
library(shinydashboard)
library(DT)
library(data.table)

##############
## Data
source('data.R', local = TRUE)

### UI ######

############################# Sidebar ###############################
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(id = "sidebar",
              menuItem("Helse enheten", tabName = "unit", icon = icon("h-square"),
                       selectInput(inputId = "unit", label = "Valg enhet",
                                   choices = c("Sykehus" = 1,
                                               "HF" = 2,
                                               "RHF" = 3),
                                   selected = 3,
                                   width = '98%')),
              menuItem("Periode", tabName = "tid", icon = icon("calendar"),
                       dateRangeInput(inputId = "date", label = "Valg dato fra og til"),
                       start = Sys.Date() - 30, end = Sys.Date(),
                       separator = "-", format = "dd-mm-yyyy", startview = "year",
                       language = "no", weekstart = 1),
              menuItem("Ulykke og skadegradering", tabName = "skade", icon = icon("medkit")),
              conditionalPanel("input.sidebar === 'skade'",
                               selectInput(inputId = "ulykkeType", "Valg type ulykke:",
                                           choices = list("Transportulykke" = 1,
                                                          "Fallulykke" = 2,
                                                          "Voldsulykke" = 3,
                                                          "Selvpåført skade" = 4,
                                                          "Arbeidsulykke" = 5,
                                                          "Sport og fritid" = 6,
                                                          "Brann og inhalasjonsskade" = 7,
                                                          "Annen ulykke" = 7),
                                           selected = 1,
                                           width = '90%')),
              menuItem("Sykehusopphod", tabName = "dag", icon = icon("bed"))
              ))


############################## Body ##################################
body <-  dashboardBody(
  tabItems(
    tabItem(tabName = "skade",
            tags$h4("  Ulykke og skadegradering"),
            fluidPage(
              fluidRow(
                box(
                  width = 4,
                  ## title = "Kroppsregion",
                  selectInput(inputId = "kropp", "Valg kroppsregion:",
                              choices = list("Head" = 1,
                                             "Face" = 2,
                                             "Neck" = 3,
                                             "Thorax" = 4,
                                             "Abdomen" = 5,
                                             "Upper extremity" = 7,
                                             "Lower extremity" = 8,
                                             "External and other" = 9),
                              selected = 1,
                              width = '99%')),
                box(
                  width = 4,
                  ## title = "Skadegradering",
                  checkboxGroupInput(inputId = "sgrad", "Valg skadegrader:",
                                     choices = list("1" = 1,
                                                    "2" = 2,
                                                    "3" = 3,
                                                    "4" = 4,
                                                    "5" = 5),
                                     inline = TRUE,
                                     selected = 2,
                                     )),
                ## Transport type
                uiOutput(outputId = "box")
              ),
              fluidRow(
                box(
                  width = 8,
                  DT::dataTableOutput("accTable")
                )
              )
            )
            ),
    tabItem(tabName = "dag",
            tags$h4("Sykehusopphold"))
  )
)



########################## Page ##################################
ui <- dashboardPage(
  dashboardHeader(title = "Nasjonalt traumeregister",
                  titleWidth = 250
                  ),
  sidebar,
  body)


######################## Server #################################
server <- function(input, output, session) {

  ## Viser transport type hvis transportulykke
  output[["box"]] <- renderUI({
    if (input$ulykkeType == 1)
      box(
        width = 4,
        selectInput(inputId = "transTyp", "Transportulykke",
                    choices = list("Bil" = 1,
                                   "MC" = 2,
                                   "Sykkel" = 3,
                                   "Båt" = 4,
                                   "Tog" = 5,
                                   "Fly" = 6,
                                   "Moped" = 7,
                                   "Annet" = 99,
                                   "Ukjent" = 999,
                                   "Alle typer" = 50),
                    selected = 50,
                    width = '98%'))
  })

  ###########################
  ## Skade og Ulykke data
  ##########################
  skadeGrad <- eventReactive(input$ulykkeType %in% 1:7, {

    ## Valgte ais-koder
    indAis <- grep("Valgte ais", names(skade)) #finne indeks til kolonne
    names(skade)[indAis] <- "ais"  #gir nytt navn til Valgte ais-koder

    ################################################
    ## kombinere alle skadekoder fra samme NTR-nr
    ## og tar bort dublikate koder
    ###############################################
    #ta bort alle missing NTR-nr.
    skade <- skade[!is.na(ntrid), ]

    ## kombinere alle skadekoder og valgt bare unike koder
    ## fra forskjellige sykehus for hver NTR-nr og variable navn blir "aiskode"
    skade[skade[!is.na(ntrid),
                toString(unique(unlist(
                  strsplit(ais, split = ",")))), by = ntrid],
          on = "ntrid", aiskode := i.V1]


    ### Beholder alle var i skadeskjema
    ### alle var starter med i. kommer fra skade skjema
    skadeUlykke <- ulykke[skade, on = "ntrid"]

    ## henter index fra acc_trans til acc_fire
    accName <- grep("acc_transport", names(skadeUlykke)):grep("acc_fire_inhal", names(skadeUlykke))
    ## convert to numeric
    for (d in accName) {
      set(skadeUlykke, j = d, value = as.numeric(skadeUlykke[[d]]))
    }

    #########################
    ## legger til HF og RHF
    #########################
    skadeUlykke[, i.UnitId := as.numeric(i.UnitId)]
    resh[, reshid := as.numeric(reshid)]

    resh[skadeUlykke, on = c(reshid = "i.UnitId")]

  })

  ## Transport typer
  transValg <- eventReactive(input$transTyp, {

    if (as.numeric(input$transTyp) == 50) {
      acd <- c(1:7, 99, 999) #valg alle transport type
    } else {
      acd <- as.numeric(input$transTyp)
    }

  })

  ## Skade og ulykke
  accData <- reactive({

    ## Data
    ## skadeGrad <- skadeGrad()
    setkey(skadeGrad(), ntrid)

    ## kroppregion
    body <- as.numeric(input$kropp)

    ## ulykke type
    accT <- as.numeric(input$ulykkeType)

    accKode <- switch(accT,
                      "acc_transport",
                      "acc_fall",
                      "acc_violence",
                      "acc_self_inflict",
                      "acc_work",
                      "acc_sprt_recreat",
                      "acc_fire_inhal",
                      "acc_other")

      ## skadegradering
      gradKode <- as.numeric(input$sgrad)

      ## transport type
      varValg <- "acc_trsp_rd_type"
      acd <- transValg()


      if (input$ulykkeType != 1){

        skadeGrad()[get(accKode) == 1 & !duplicated(ntrid) & !is.na(ntrid),
                    list(ja = ifelse(sum(grepl(
                    paste0("^", body, ".*[", paste(gradKode, collapse = ""), "]$"),
                    as.numeric(unlist(
                      strsplit(aiskode, split = ","))))) != 0, 1, 0),
                    sykehus = i.HealthUnitName,
                    hf = HF,
                    rhf = RHF), #bruk i.HealthUnitName som kommer fra skadeskjema
                  by = c("ntrid")]
      } else {

        skadeGrad()[get(varValg) %in% acd & !duplicated(ntrid) & !is.na(ntrid),
                    list(ja = ifelse(
                    sum(grepl(paste0("^", body, ".*[", paste(gradKode, collapse = ""), "]$"),
                              as.numeric(unlist(
                                strsplit(aiskode, split = ","))))) != 0, 1, 0),
                    sykehus = i.HealthUnitName,
                    hf = HF,
                    rhf = RHF), #bruk i.HealthUnitName som kommer fra skadeskjema
                  by = c("ntrid")]

      }
    })

  ## Table for Ullyke og skadegradering
  output$accTable <- DT::renderDataTable({

    unit <- switch(as.numeric(input$unit),
                   "sykehus",
                   "hf",
                   "rhf")

    accData()[ja == 1, .N, by = unit]

  })

  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
