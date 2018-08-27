## Input
ukeDagInput <- function(id){
  ns <- NS(id)

  tagList(
    uiOutput(ns("hosp"))

  )
}


ukeDag <- function(input, output, session, data){

  ns <- session$ns

  helseEnhet <- as.factor(unique(data$Hospital))

  output$hosp <- renderUI({
    selectInput("hosp_in", label = NULL, choices = sort(helseEnhet))
  })

  return(helseEnhet)
}
