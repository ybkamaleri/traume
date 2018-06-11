## dynamisk input basert på innhold i kolonne

library(shiny)
library(data.table)

set.seed(1234)

DT <- data.table(
  v1 <- c("Nord", "Midt", "Vest", "Sør", "Sør", "Nord", "Nord", "Nord"),
  v2 <- sample(2:4, 8, replace = TRUE)
)


server <- function(input, output) {

  output$sykehus <- renderUI({
    selectInput("nama", "List sykehus", as.list(unique(DT$V1)))
  })

}


ui <- fluidPage(
  titlePanel("Test dynamisk input"),

  sidebarPanel(
    uiOutput("nama")

  ),

  mainPanel(

    verbatimTextOutput("sykehus")
  )
)


shinyApp(ui, server)
