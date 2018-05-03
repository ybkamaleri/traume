
library(shiny)
library(shinydashboard)

##############
## Data
source('data.R', local = TRUE)

### UI ######

############################# Sidebar ###############################
sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(id = "sidebar",
              menuItem("Helse enheten", tabName = "unit", icon = icon("building"),
                       selectInput(inputId = "unit", label = "Valg enhet",
                                   choices = c("Sykehus" = 1,
                                               "HF" = 2,
                                               "RHF" = 3),
                                   selected = 3,
                                   width = '98%')),
              menuItem("Periode", tabName = "tid", icon = icon("calendar"),
                       dateRangeInput(inputId = "date", label = "Valg dato fra og til"),
                       start = Sys.Date() - 30, end = Sys.Date(),
                       separator = "-", format = "dd-mm-yyyy", startview = "year",
                       language = "no", weekstart = 1),
              menuItem("Type ulykke", tabName = "skade", icon = icon("medkit")),
              conditionalPanel("input.sidebar === 'skade'",
                               selectInput("ulykke1", "Valg type ulykke:",
                                           choices = list("Transportulykke" = 1,
                                                          "Fallulykke" = 2,
                                                          "Voldsulykke" = 3,
                                                          "Arbeidsulykke" = 4,
                                                          "Sport og fritid" = 5,
                                                          "Brann og inhalasjonsskade" = 6,
                                                          "Annen ulykke" = 7),
                                           selected = 7,
                                           width = '90%')),
              menuItem("Sykehusopphod", tabName = "dag", icon = icon("bed"))
              ))


############################## Body ##################################
body <-  dashboardBody(
  tabItems(
    tabItem(tabName = "skade",
            h2("Type ulykke og skadegradering"),
            fluidRow(
              box(
                width = 4,
                title = "Type ulykke",
                selectInput("ulykke", "Valg type ulykke:",
                            choices = list("Transportulykke" = 1,
                                             "Fallulykke" = 2,
                                             "Voldsulykke" = 3,
                                             "Arbeidsulykke" = 4,
                                             "Sport og fritid" = 5,
                                             "Brann og inhalasjonsskade" = 6,
                                             "Annen ulykke" = 7),
                            selected = 7,
                            width = '90%')),

              box(
                width = 4,
                title = "Kroppsregion",
                selectInput("kropp", "Valg kroppsregion:",
                            choices = list("Head" = 1,
                                           "Face" = 2,
                                           "Neck" = 3,
                                           "Thorax" = 4,
                                           "Abdomen" = 5,
                                           "Upper extremity" = 7,
                                           "Lower extremity" = 8,
                                           "External and other" = 9),
                            selected = 1,
                            width = '90%')),
              box(
                width = 4,
                title = "Skadegradering",
                checkboxGroupInput("skade", "Valg skadegrader:",
                                   choices = list("1" = 1,
                                                  "2" = 2,
                                                  "3" = 3,
                                                  "4" = 4,
                                                  "5" = 5),
                                   )),
              box(
                dataTableOutput("table"),
                textOutput("skadegrad")

              )
              ## ,
              ## box(
              ##   textOutput("skadegrad")
              ## )
              )),
    tabItem(tabName = "dag",
            h2("Sykehusopphold")))
)



########################## Page ##################################
ui <- dashboardPage(
  dashboardHeader(title = "Nasjonalt traumeregister",
                  titleWidth = 300
                  ),
  sidebar,
  body)


######################## Server #################################
server <- function(input, output, session) {

  output$table <- renderDataTable({
    head(masterID)
  })

  output$skadegrad <- renderText({
    skadeTall <- input$skade
  })
}

shinyApp(ui, server)
