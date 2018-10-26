
## Module for filtrering data
#############################

#### UI #####
filterUI <- function(id){

  ns  <- NS(id)

  fluidPage(
    fluidRow(
      box(width = 3, height = 165, status = "info",
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

      box(width = 3, height = 165, status = "info",
        dateRangeInput(inputId = ns("tidsrom_in"),
          label = "Valg dato fra og til",
          start = Sys.Date() - 360, #alt. min date
          end = Sys.Date(),
          separator = "til",
          format = "dd.mm.yyyy",
          startview = "month",
          language = "no",
          weekstart = 1)),

      box(width = 3, height = 165, status = "info",
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
        tags$h4("Bruk valgte spesifikasjoner?"),
        column(
          id = "Butang",
          width = 6,
          actionButton(ns("runButton"), label = "OK")
        ),
        column(
          id = "Butang",
          width = 6,
          actionButton(ns("resetButton"), label = "Reset")
        ),

        ## actionButton(ns("runButton"), label = "OK",
        ##   style = 'padding:5px 30px; border: none; text-align: center; font-size:15px;' ),
        htmlOutput(ns("txtList"))
      )
    ),

    fluidRow(
      box(width = 9,
        tabBox(side = 'left', selected = "Figur", width = 12,
          tabPanel("Figur", plotOutput(ns("fig"))),
          tabPanel("Tabell", DT::dataTableOutput(ns("tabell"))))
      ),

      ## Tall Info
      box(id = "filtInfo", width = 3,
        textOutput(ns("kilde")),
        textOutput(ns("traume_info")),
        textOutput(ns("mann_info")),
        textOutput(ns("kvinne_info")))
    )
  )
}

########################
####### SERVER #########
########################

filterSV <- function(input, output, session, resh, data){

  ns <- session$ns



  ## Dynamisk input
  ## ==============
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

    unitLength <- navnLevel()
    valgUnit <- unitLength[sample(1:length(unitLength), 1)]
    updateSelectInput(session, "valgLevel02",
      label = valgNavn(),
      choices = sort(navnLevel()),
      selected = valgUnit)
  })

  ## Tekst for utvalg filter
  output$txtList <- renderUI({

    if (input$valgLevel01 == 1){
      valgUnit <-  paste0("Velger data for hele landet")
    }else{
      valgUnit <- paste0("Velger data for ", input$valgLevel02)
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



  ## text til videre visning
  ## =======================
  txt <- reactive({

    if (input$valgLevel01 == 1){
      valgUnit <- paste0("Data for hele landet")
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

    txtUT <- paste0(valgUnit, br(), valgTid, br(), valgAge)
    txtUT

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

  ## Reset Button
  ## =============
  initialInputs <- isolate(reactiveValuesToList(input))

  observe({
    # OPTIONAL - save initial values of dynamic inputs
    inputValues <- reactiveValuesToList(input)
    initialInputs <<- utils::modifyList(inputValues, initialInputs)
  })

  observeEvent(input$resetButton, {
    for (id in names(initialInputs)) {
      value <- initialInputs[[id]]
      # For empty checkboxGroupInputs
      if (is.null(value)) value <- ""
      session$sendInputMessage(id, list(value = value))
    }
  })


  ## Bruk funksjon fra fil plotAgeSex
  dataUT <- reactive({

    if (input$alder_kat){
      valgUT <- fun.plotAS(dataFil(), TRUE)
    } else {
      valgUT <- fun.plotAS(dataFil())
    }
    valgUT
  })

  ## Viser eller skjulder figur og tekst
  ## ===================================
  gg <- reactiveValues(visPlot = FALSE)

  observeEvent(input$runButton, {
    gg$visPlot <- input$runButton
  })

  observeEvent(input$resetButton, {
    gg$visPlot <- FALSE
  })


  ## Info
  ###################
  ## Data kilder som brukes og vises
  outKilde <- reactive({
    if (input$valgLevel01 == 1){
      valgUnit <-  paste0("Tall for hele landet")
    }else{
      valgUnit <- paste0("Tall for ", input$valgLevel02)
    }
  })

  output$kilde <- renderText({

    if (gg$visPlot == FALSE) return()

    isolate({
      outKilde()
    })
  })

  output$traume_info <- renderText({

    if (gg$visPlot == FALSE) return()
    isolate({
      paste0("Antall traume: ", uniqueN(dataFil()$ntrid))
    })
  })

  output$mann_info <- renderText({

    if (gg$visPlot == FALSE) return()

    isolate({
      data <- dataFil()[!duplicated(ntrid) & gender == 1, .N]
      paste0("Antall menn: ", data)
    })
  })

  output$kvinne_info <- renderText({

    if (gg$visPlot == FALSE) return()
    isolate({
      data <- dataFil()[!duplicated(ntrid) & gender == 2, .N]
      paste0("Antall kvinner: ", data)
    })
  })

  ## Plot alder og kjønn
  output$fig <- renderPlot({
    if (gg$visPlot == FALSE) return()

    isolate({
      plotUT <- dataUT()$plot
      print(plotUT)
    })

  })

  ## Tabell for alder og kjønn
  output$tabell <- DT::renderDT({
    if (gg$visPlot == FALSE) return()
    isolate({
      dataUT()$data
    })
  })

  ## return data
  var <- reactiveValues()

  observeEvent(input$runButton,{
    var$data <- dataFil()
    var$txt <- txt()
  })

  return(var)
  ## return(reactive({data <- dataFil()}))

}
