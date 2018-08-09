## dynamisk input basert på innhold i kolonne

library(shiny)
library(data.table)

set.seed(1234)

DT <- data.table(
  V1 <- c("Nord", "Midt", "Vest", "Sør", "Sør", "Nord", "Nord", "Nord"),
  V2 <- c("Satu", "Dua", "Tiga", "Empat"),
  V3 <- c("Ali", "Baba", "Nonya", "Baba", "Nonya")
)


#################
## Vanlig shiny
##################
server <- function(input, output) {
  ## ## denne funker også men gjøres på annen måte
  ## namasyk <- eventReactive(input$nom, {
  ##   if (input$nom == 1){
  ##     namasyk <- unique(DT$V1)
  ##   } else {
  ##     namesyk <- unique(DT$V2)
  ##   }
  ## })

  ## namasyk <- eventReactive(input$nom, {

  ##   ifelse(input$nom == 1, unique(DT$V1),
  ##          ifelse(input$nom == 2, unique(DT$V2),
  ##                 unique(DT$V4)))
  ## })

  namasyk <- eventReactive(input$nom, {

    namasyk <- switch(as.character(input$nom),
                      '1' = unique(DT$V1),
                      '2' = unique(DT$V2),
                      '3' = unique(DT$V3))

  })


  output[["syke2"]] <- renderUI({

    nama <- as.list(namasyk())
    selectInput("sykeIn", "Valg Sykehus", choices = nama)

  })

  output$syk3 <- renderText(input$sykeIn)
}


ui <- fluidPage(
  titlePanel("Test dynamisk input"),

  sidebarPanel(
    selectInput("nom", "Pilih nombor", choices = 1:3 ),
    uiOutput("syke2")

  ),

  mainPanel(
    textOutput("syk3")
  )
)


shinyApp(ui, server)


#######################################
## Module
#######################################

valgItem <- function(input, output, session, valg){

  output$HelseEnhet <- renderUI({
    selectInput(session$ns("enhetInput", "Valg Enhet", choices = ))
  })

}
