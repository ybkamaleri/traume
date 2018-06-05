
######################## Server #################################
server <- function(input, output, session) {

  ########## generell info #######################
  output$ntrAntall <- renderValueBox({
    valueBox(
      length(unique(masterID$ntrid)), "Antall traume",
      icon = icon("male"),
      color = "blue"
    )
  })

  output$ntrAlvor <- renderValueBox({
    valueBox(
      value = 3000,
      subtitle = "Antall NISS≥16",
      icon = icon("heartbeat"),
      color = "blue"
    )
  })

  output$ntrDie <- renderValueBox({
    valueBox(
      value = paste0(12.1, "%"),
      subtitle = "Andel døde innen 30 dagers",
             icon = icon("user-circle-o"),
      color = "blue"
    )
  })

  output$demoGender <- renderPlotly({

    ## bytt NA med 0
    bNA <- function(DT, na = 0){
      for (j in seq_len(ncol(DT)))
        set(DT,which(is.na(DT[[j]])),j, na)
    }


    traume[PatientAge == -1, PatientAge := NA] #bytt -1 til NA
    ## finner ut hvordan velger man -1 f.eks PatientAge!=-1 ikke funker. Koden nedenfor eksluderer ikke -1
    ## demo <- traume[PatientAge > -1 | !is.na(PatientAge) , .N, keyby = list(PatientGender, PatientAge)]

    demo <- traume[!is.na(PatientAge) , .N, keyby = list(PatientGender = as.factor(PatientGender), PatientAge = as.numeric(PatientAge))]
    ## Ta bort missing PatientAge
    ageMale <- demo[PatientGender == 1 & !is.na(PatientAge),
                    list(Alder = PatientAge, menn = N)]
    ageFemale <- demo[PatientGender == 2 & !is.na(PatientAge),
                      list(Alder = PatientAge, kvinner = N)]
    ## ageMF <- merge(ageMale, ageFemale, by.x = "Alder", by.y = "Alder")
    ageMF <- ageMale[ageFemale, on = c(Alder = "Alder")]
    bNA(ageMF)
    ageMF[, alle := menn + kvinner]

    ageDemo <- ggplot(ageMF, aes(x = Alder)) +
      geom_line(aes(y = menn, color = "Menn"), size = 1) +
      geom_line(aes(y = kvinner, color = "Kvinner"), size = 1) +
      geom_line(aes(y = alle, color = "Begge"), size = 1) +
      ## title(xlab = "Alder", ylab = "Antall") +
      xlab("Alder") +
      ylab("Antall") +
      theme_minimal() +
      scale_color_manual(name = NULL,
                         values = c(Menn = "blue", Kvinner = "lightblue", Begge = "orange"))

    gg <- ggplotly(ageDemo)
    rangeslider(gg)

  })

  ## Viser transport type hvis transportulykke
  output[["typeTrans"]] <- renderUI({
    if (input$ulykkeType == 1)
      box(
        width = 4,
        selectInput(inputId = "transTyp", "Valg transport type:",
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

  ####################
  ##  Ulykke typer  ##
  ####################
  observeEvent(input$ulykkeType, {

    frameTitle <- switch(as.numeric(input$ulykkeType),
                         "Transportulykke",
                         "Fallulykke",
                         "Voldsulykke",
                         "Selvpåført skade",
                         "Arbeidsulykke",
                         "Sport og fritid relaterte skade",
                         "Brann og inhalasjonsskade",
                         "Annen type ulykke")

    output$titleUT <- renderText(frameTitle)
  })


  ##########################
  ## Skade og Ulykke data ##
  ##########################
  skadeGrad <- eventReactive(input$ulykkeType, {

    ## Valgte ais-koder
    indAis <- grep("Valgte ais", names(skade)) #finne indeks til kolonne
    names(skade)[indAis] <- "ais"  #gir nytt navn til Valgte ais-koder

    ## - ta bort alle missing NTR-nr.
    skade <- skade[!is.na(ntrid), ]

    ## kombinere alle skadekoder og valgt bare unike koder
    ## fra forskjellige sykehus for hver NTR-nr og variable navn blir "aiskode"
    ## referer til mitt spørsmål på Stackoverflow
    skade[skade[, toString(unique(unlist(strsplit(ais, split = ",")))),
                by = ntrid],
          on = "ntrid", aiskode := i.V1]

    ### Kombinere skade og ulykke skjemaer
    ### Beholder alle var i skadeskjema
    ### alle var starter med i. kommer fra skade skjema
    skadeUlykke <- ulykke[skade, on = "ntrid"]

    ## henter index fra acc_trans til acc_fire
    accName <- grep("acc_transport", names(skadeUlykke)):grep("acc_fire_inhal", names(skadeUlykke))
    ## convert to numeric
    for (d in accName) {
      set(skadeUlykke, j = d, value = as.numeric(skadeUlykke[[d]]))
    }


    #### legger til HF og RHF ####
    skadeUlykke[, i.UnitId := as.numeric(i.UnitId)]
    resh[, reshid := as.numeric(reshid)]

    resh[skadeUlykke, on = c(reshid = "i.UnitId")]

  })

  #####################
  ## Transport typer ##
  #####################
  transValg <- eventReactive(input$transTyp, {

    if (as.numeric(input$transTyp) == 50) {
      acd <- c(1:7, 99, 999) #valg alle transport type
    } else {
      acd <- as.numeric(input$transTyp)
    }

  })


  #####################
  ## Skade og ulykke ##
  #####################

  accData <- reactive({

    ## Data
    ## skadeGrad <- skadeGrad()
    setkey(skadeGrad(), ntrid)

    ## kroppregion
    inBody <- as.numeric(input$kropp)
    body <- paste(inBody, collapse = "")

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

      ## Input for skadegradering
      gradKode <- as.numeric(input$sgrad)

      ## Numeric type for transport type fra en reactive funksjon
      varValg <- "acc_trsp_rd_type"
      acd <- transValg()

      ## Ja hvis minst en kroppsregion er skadet
      if (input$ulykkeType != 1){

        skadeGrad()[get(accKode) == 1 & !duplicated(ntrid) & !is.na(ntrid),
                    list(ja = ifelse(sum(grepl(
                      paste0("^", paste0("[", body, "]"),
                             ".*[", paste(gradKode, collapse = ""), "]$"),
                      as.character(unlist(
                        strsplit(aiskode, split = ","))))) != 0, 1, 0),
                      Sykehus = i.HealthUnitName,
                      HF = HF,
                      RHF = RHF), #bruk i.HealthUnitName som kommer fra skadeskjema - spør dem om dette er riktig
                    by = c("ntrid")]
      } else {

        skadeGrad()[get(varValg) %in% acd & !duplicated(ntrid) & !is.na(ntrid),
                    list(ja = ifelse(
                      sum(grepl(paste0("^", paste0("[", body, "]"),
                                       ".*[", paste(gradKode, collapse = ""), "]$"),
                                as.character(unlist(
                                  strsplit(aiskode, split = ","))))) != 0, 1, 0),
                      Sykehus = i.HealthUnitName,
                      HF = HF,
                      RHF = RHF), #bruk i.HealthUnitName som kommer fra skadeskjema
                    by = c("ntrid")]

      }
    })


  ###############################################
  ## Antall for hver ulykkeType untatt transport
  ###############################################
  accAntall <- eventReactive(input$ulykkeType, {
    dim(accData())[1]
  })

  output[["accInfo"]] <- renderUI({
    if (input$ulykkeType != 1)
      box(
        width = 4, background = "navy",
        tags$h3(paste0("Total = ", accAntall()))
      )
  })

  #######################################################
  ## Info om total antall for hver transport ulykke type
  #######################################################
  transAntall <- eventReactive(input$transTyp, {
    dim(accData())[1]

  })

  output[["transInfo"]] <- renderUI({
    if (input$ulykkeType == 1)
      box(
        width = 4, background = "navy",
        tags$h3(paste0("Total = ", transAntall()))
      )
  })



  ########################################
  ## Table for Ullyke og skadegradering ##
  ########################################

  output$accTable <- DT::renderDataTable({

    unit <- switch(as.numeric(input$unit),
                   "Sykehus",
                   "HF",
                   "RHF")
    alle <- dim(accData())[1]
    tabAcc <- accData()[ja == 1, .N, keyby = unit]
    tabAcc[, prosent := round((N / alle) * 100, digits = 2), keyby = unit]

  })

  session$onSessionEnded(stopApp)
}

## shinyApp(ui, server)
