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

  regdata <- eventReactive(input$input_filter, {

  })

  session$onSessionEnded(stopApp)
}
