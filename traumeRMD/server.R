function(input, output, session){

  ## ValueBox
  ####################################
  ## Antall traume for valueBox
  sumTraume <- uniqueN(masterFile$ntrid)

  ## valueBox Antall traume
  output$o_traume <- renderValueBox({
    valueBox(
      value = sumTraume,
      subtitle = "Antall traume",
      icon = icon("pie-chart"),
      color = "blue"
    )
  })

  ## valueBox niss > 15
  output$o_ais <- renderValueBox({
    valueBox(
      value = skade[!duplicated(ntrid) & inj_niss > 15, .N],
      subtitle = "Antall NISS > 15",
      icon = icon("heartbeat"),
      color = "blue"
    )
  })

  ## valueBox døde innen 30 dager
  output$o_dead <- renderValueBox({
    valueBox(
      value = intensiv[!duplicated(ntrid) & res_survival == 1, .N],
      subtitle = "Antall registert døde innen 30 dager",
      icon = icon("bed"),
      color = "blue"
    )
  })


  ## Dygraphs for antall traume with timeseries
  ##################################################
  output$traume_dygraph <- renderDygraph({
    dataDG <- copy(masterFile)
    dataDG[, dateAll := as.Date(dateAll)]

    ## Time-series antall traume per dag
    dateAll_dt <- dataDG[!is.na(dateAll), .N, by = .(dateAll)]
    dateMann_dt <- dataDG[!is.na(dateAll)][gender == 1,.N, by = .(dateAll)]
    dateKvinne_dt <- dataDG[!is.na(dateAll)][gender == 2,.N, by = .(dateAll)]

    tsTraumeSub <- dateMann_dt[dateAll_dt, on = c(dateAll = "dateAll")]
    tsTraumeAll <- dateKvinne_dt[tsTraumeSub, on = c(dateAll = "dateAll")]

    ## replace NA to 0 with function by number - RASKERE!
    rep0 <- function(DT){
      for (j in seq_len(ncol(DT)))
        set(DT, which(is.na(DT[[j]])),j,0)
    }
    rep0(tsTraumeAll)

    ## Konvertere til timeseries
    tsAlle <- xts::xts(tsTraumeAll$i.N.1, order.by = tsTraumeAll$dateAll)
    tsMann <- xts::xts(tsTraumeAll$i.N, order.by = tsTraumeAll$dateAll)
    tsKvinne <- xts::xts(tsTraumeAll$N, order.by = tsTraumeAll$dateAll)

    timeTraumeAlle <- cbind(tsAlle, tsMann, tsKvinne)

    ## maxDato og minDato for dyRangeSelector()
    maxDato <- strftime(max(dateAll_dt$dateAll))
    minDato <- strftime(zoo::as.yearqtr(as.Date(maxDato)) - 1, frac = 2)

    ## library(dygraphs)
    dygraph(timeTraumeAlle,
      main = "Antall Traume per dag og kjønn",
      ylab = "Antall") %>%
      dySeries("..1", label = "Alle") %>%
      dySeries("..2", label = "Menn") %>%
      dySeries("..3", label = "Kvinner") %>%
      dyHighlight(highlightCircleSize = 5,
        highlightSeriesBackgroundAlpha = 0.3,
        hideOnMouseOut = FALSE) %>%
      dyLegend(show = "always", hideOnMouseOut = FALSE, width = 280) %>%
      dyRangeSelector(dateWindow = c(minDato, maxDato))

  })


  ########################################################

  ## Filter data
  #######################
  dataClean <- callModule(filterSV, "dataFilter", resh, masterFile)


  ## Skadegradering
  ##################
  callModule(module = skadeSV,
    id = "skade",
    valgDT = dataClean, # masterFile for TEST DATA!!!
    dataUK = ulykke,
    dataSK = skade)

  ## Virksomhetsdata på sykehus
  ###############################
  valgHosp <- callModule(virkDataSV, "virkData", resh)
  callModule(virkPlotSV, "virkPlot", valg = valgHosp, data = akutt)

  ## Standard Rapport
  #######################
  ## callModule(rapportSV, "rapport", resh, data = akutt)

  hospValg <- sort(as.factor(unique(resh$Hospital)))
  hospRan <- sample(1:length(hospValg), 1)


  
  output$hosp <- renderUI({
    selectInput("hosp_rapport",
      label = NULL,
      choices = hospValg,
      selected = hospValg[hospRan]
    )
  })

  ## Tittle og subtitle
  hospNavn <- reactive({ input$hosp_rapport})

  datoFra <- reactive({ input$dato_rapport[1] })
  datoTil <- reactive({ input$dato_rapport[2] })

  ## observeEvent(input$dato_rapport, {
  ##   datoRange <- paste0(format(as.Date(datoFra()), '%d.%m.%Y'), " til ", format(as.Date(datoTil()), '%d.%m.%Y'))
  ## })

  ## datoRange <- eventReactive(input$dato_rapport, {

  ##   datoFra = input$dato_rapport[1]
  ##   datoTil = input$dato_rapport[2]

  ##   dato <- paste0(format(as.Date(datoFra()), '%d.%m.%Y'), " til ", format(as.Date(datoTil()), '%d.%m.%Y'))

  ##   return(dato)
  ## })

  ## datoRange <- "Test 007"

  output$downloadReport <- downloadHandler(
    filename = function() {
      paste0(hospNavn(), Sys.Date(), ".pdf")
    },

    content = function(file) {
      src <- normalizePath('doc/ntrRapport.rmd')

      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(src, 'ntrRapport.rmd', overwrite = TRUE)

      library(rmarkdown)
      out <- render('ntrRapport.rmd',
        params = list(
          dynamictitle = hospNavn(),
          reportdate = Sys.Date(),
          dynamicSub = paste0(datoFra(), " til ", datoTil()))
      )

      file.rename(out, file)
    }
  )

  ## HTML side
  ## ==================
  ## htmlSide <- function(){
  ##   return(includeHTML("./index.html"))
  ## }

  output$txtSide <- renderUI({

    traumeSide <- "https://ybkamaleri.github.io/traume/"
    htmlSide <- tags$iframe(src = traumeSide, height = "750", width = "100%")
    print(htmlSide)
    htmlSide
  })

  ## Test
  ## ====================
  output$test <- renderPrint({
    hospNavn()
  })

  session$onSessionEnded(stopApp)
}
