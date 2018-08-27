
ukeDagInput <- function(id){
  ns <- NS(id)

  tagList(
    uiOutput(ns("hosp")),

    ## Dato range
    dateRangeInput(inputId = ns("virk_date_in"),
                   label = "Valg dato fra og til",
                   start = Sys.Date() - 30, #alt. min date
                   end = Sys.Date(),
                   separator = "til",
                   format = "dd-mm-yyyy",
                   startview = "month",
                   language = "no",
                   weekstart = 1)

  )
}


ukeDag <- function(input, output, session, data){
  ns <- session$ns
  helseEnhet <- as.factor(unique(data$Hospital))

  ## list sykehusnavn
  output$hosp <- renderUI({
    selectInput(ns("hosp_in"),
                label = NULL,
                choices = sort(helseEnhet),
                selected = sort(helseEnhet)[1])
  })

  ## Sykehusnavn
  return(list(value = reactive({input$hosp_in}),
              datoFra = reactive({input$virk_date_in[1]}),
              datoTil = reactive({input$virk_date_in[2]})))

}
