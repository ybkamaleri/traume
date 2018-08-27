
ukeDagInput <- function(id){
  ns <- NS(id)

  tagList(
    uiOutput(ns("hosp"))

  )
}


ukeDag <- function(input, output, session, data){
  ns <- session$ns
  helseEnhet <- as.factor(unique(data$Hospital))

  ## list sykehusnavn
  output$hosp <- renderUI({
    selectInput(ns("hosp_in"), label = NULL, choices = sort(helseEnhet))
  })

  ## Sykehusnavn
  return(list(value = reactive({input$hosp_in})))

}
