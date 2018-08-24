function(input, output, session) {

  ## Valg analysenivå
  AnalyseLevel  <- reactive({
    switch(as.character(input$analyse_level_in),
           '2' = unique(resh$RHF),
           '3' = unique(resh$HF),
           '4' = unique(resh$Hospital))
  })

  ## Output analysenivå
  output[["health_level_out"]] <- renderUI({
    helseEnhet <- as.list(AnalyseLevel())
    selectInput("health_level_in", label = NULL, choices = helseEnhet)
  })

  ## Filtrerer data for Enhet og Dato
  filterData <- reactive({

    datoFra <- as.character(input$tidsrom_in[1])
    datoTil <- as.character(input$tidsrom_in[2])

    paste0("Valgte tidsrom: ",
           format(as.Date(datoFra, format = "%Y-%m-%d"), "%d/%m/%Y"),
           " til ",
           format(as.Date(datoTil, format = "%Y-%m-%d"), "%d/%m/%Y"))
  })

  ## HelseEnhet
  enhetNavn <- reactive({input$health_level_in})


  ## Aldersgruppe
  ageRange <- reactive({

    ageFra <- input$alder_in[1]
    ageTil <- input$alder_in[2]

    paste0(ageFra, " til ", ageTil)

  })

  ## TEST
  output$test <- renderText({
    enhetNavn()
  })

  ## Dygraphs for antall traume with timeseries
  ##################################################
  output$traume_dygraph <- renderDygraph({
    ## Time-series antall traume per dag
    dateAll_dt <- masterFile[!is.na(dateAll), .N, by = .(dateAll)]
    dateMann_dt <- masterFile[!is.na(dateAll)][gender == 1,.N, by = .(dateAll)]
    dateKvinne_dt <- masterFile[!is.na(dateAll)][gender == 2,.N, by = .(dateAll)]

    tsTraumeSub <- dateMann_dt[dateAll_dt, on = c(dateAll = "dateAll")]
    tsTraumeAll <- dateKvinne_dt[tsTraumeSub, on = c(dateAll = "dateAll")]

    ## replace NA to 0 with function by number - RASKERE!
    rep0 <- function(DT){
      for (j in seq_len(ncol(DT)))
        set(DT, which(is.na(DT[[j]])),j,0)
    }
    rep0(tsTraumeAll)


    library(xts)
    tsAlle <- xts::xts(tsTraumeAll$i.N.1, order.by = tsTraumeAll$dateAll)
    tsMann <- xts::xts(tsTraumeAll$i.N, order.by = tsTraumeAll$dateAll)
    tsKvinne <- xts::xts(tsTraumeAll$N, order.by = tsTraumeAll$dateAll)

    timeTraumeAlle <- cbind(tsAlle, tsMann, tsKvinne)

    ## maxDato og minDato for dyRangeSelector()
    maxDato <- strftime(max(dateAll_dt$dateAll))
    minDato <- strftime(zoo::as.yearqtr(as.Date(maxDato)) - 1, frac = 2)

    ## install.packages("dygraphs")
    library(dygraphs)
    dygraph(timeTraumeAlle,
            main = "Antall Traume per dag og kjønn",
            ylab = "Antall") %>%
      dySeries("..1", label = "Alle") %>%
      dySeries("..2", label = "Menn") %>%
      dySeries("..3", label = "Kvinner") %>%
      dyHighlight(highlightCircleSize = 5,
                  highlightSeriesBackgroundAlpha = 0.3,
                  hideOnMouseOut = FALSE) %>%
      dyLegend(show = "always", hideOnMouseOut = FALSE, width = 500) %>%
      dyRangeSelector(dateWindow = c(minDato, maxDato))
  })


  ## Antall traume for valueBox
  sumTraume <- masterFile[unique(ntrid), .N]

  ## valueBox Antall traume
  output$o_traume <- renderValueBox({
    valueBox(
      value = sumTraume,
      subtitle = "Antall traume",
      icon = icon("male"),
      color = "blue"
    )
  })

  ## valueBox niss > 15
  output$o_ais <- renderValueBox({
    valueBox(
      value = skade[unique(ntrid),][inj_niss > 15, .N],
      subtitle = "Antall NISS > 15",
      icon = icon("heartbeat"),
      color = "blue"
    )
  })

  ## valueBox døde innen 30 dager
  output$o_dead <- renderValueBox({
    valueBox(
      value = intensiv[unique(ntrid)][res_survival == 1, .N],
      subtitle = "Antall registert døde innen 30 dager",
      icon = icon("bed"),
      color = "blue"
    )
  })

  ## Virksomhetsdata på sykehus
  output[["virk_sykehus_out"]] <- renderUI({
    helseEnhet <- as.list(unique(resh$Hospital))
    selectInput("virk_sykehus_in", label = NULL, choices = helseEnhet)
  })

  output$test2 <- renderText({
    paste0(input$virk_sykehus_in)
  })

  session$onSessionEnded(stopApp)
}
