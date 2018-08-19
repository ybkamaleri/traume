function(input, output, session) {

  ## Valg analysenivå
  AnalyseLevel  <- eventReactive(input$input_analyse_level, {
    switch(as.character(input$input_analyse_level),
           '2' = unique(resh$RHF),
           '3' = unique(resh$HF),
           '4' = unique(resh$Sykehus))
  })

  ## Output analysenivå
  output[["output_health_level"]] <- renderUI({
    helseEnhet <- as.list(AnalyseLevel())
    selectInput("input_health_level", label = "Valg enhet", choices = helseEnhet)
  })

  ## Filtrerer data for Enhet og Dato
  filterData <- eventReactive(input$input_filter, {

    datoFra <- as.character(input$input_date[1])
    datoTil <- as.character(input$input_date[2])

    paste0(datoFra, " til ", datoTil)

  })

  ## Test output
  output$test <- renderPrint({filterData()})

  ## valueBox Antall traume
  output$o_traume <- renderValueBox({
    valueBox(
      value = masterFile[unique(ntrid), .N],
      subtitle = "Antall traume",
      icon = icon("male"),
      color = "blue"
    )
  })

  ## valueBox niss > 15
  output$o_ais <- renderValueBox({
    valueBox(
      value = 3000,
      subtitle = "Antall NISS > 15",
      icon = icon("heartbeat"),
      color = "blue"
    )
  })

  ## valueBox døde innen 30 dager
  output$o_dead <- renderValueBox({
    valueBox(
      value = paste0(12.1, "%"),
      subtitle = "Andel døde innen 30 dager",
      icon = icon("user-circle-o"),
      color = "blue"
    )
  })

  session$onSessionEnded(stopApp)
}
