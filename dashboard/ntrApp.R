## UI
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "NTR"),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output) {

}

shinyApp(ui, server)
