## Subset data
valg_Input <- function(id, label, choices = NULL, multiple = FALSE){
  ns <- NS(id)
  selectInput(ns("subset_input"), label,
              choices = choices,
              multiple = multiple)
}

valg_server <- function(input, output, session, regdata, varname){



}
