## Virksomhetsrapport module
############################

virk_ModUI <- function(id){

  ns <- NS(id)

  fluidRow(
    ## Valg hospital
    box(width = 3,
        height = 100,
        title = "Valg sykehus:",
        status = "primary",
        solidHeader = TRUE,
        uiOutput(ns("hosp"))),
    ## Valg tidsrom
    box(width = 3,
        height = 100,
        title = "Valg tidsrom:",
        status = "primary",
        solidHeader = TRUE,
        dateRangeInput(inputId = ns("virk_date_in"),
                       label = NULL,
                       start = Sys.Date() - 30, #alt. min date
                       end = Sys.Date(),
                       separator = "til",
                       format = "dd-mm-yyyy",
                       startview = "month",
                       language = "no",
                       weekstart = 1))
  )
}

virk_Mod <- function(input, output, session, resh) {

  helseEnhet <- as.factor(unique(resh$Hospital))

  ## list sykehusnavn
  output$hosp <- renderUI({
    ns <- session$ns
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
