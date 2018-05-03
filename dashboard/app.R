
library(shiny)
library(shinydashboard)

### UI ######

## Sidebar ###
sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    menuItem("Helse enheten", tabName = "unit", icon = icon("building"),
             selectInput(inputId = "unit", label = "Valg enhet",
                         choices = c("Sykehus", "HF", "RHF"),
                         width = '98%')),
    menuItem("Periode", tabName = "tid", icon = icon("calendar"),
             dateRangeInput(inputId = "date", label = "Valg dato fra og til"),
             start = Sys.Date() - 30, end = Sys.Date(),
             separator = "-", format = "dd-mm-yyyy", startview = "year",
             language = "no", weekstart = 1),
    menuItem("Ulykke og Skadegradering", tabName = "skade", icon = icon("medkit")),
    menuItem("Sykehusopphod", tabName = "dag", icon = icon("bed"))
  ))


### Body ###
body <-  dashboardBody(
  tabItems(
    tabItem(tabName = "skade",
            h2("Type ulykke og skadegradering"),
            fluidRow(
              box(
                title = "Type ulykke",
                selectInput("ulykke", "Valg type ulykke:",
                            choices = list("Transportulykke" = 1,
                                           "Fallulykke" = 2,
                                           "Voldsulykke" = 3,
                                           "Arbeidsulykke" = 4,
                                           "Sport og fritid" = 5,
                                           "Brann og inhalasjonsskade" = 6,
                                           "Annen ulykke" = 7),
                            selected = 7)
              ),
              box(
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
                            selected = 1))
            )),
    tabItem(tabName = "dag",
            h2("Sykehusopphold")))
)



## Page ##
ui <- dashboardPage(
  dashboardHeader(title = "Nasjonalt traumeregister",
                  titleWidth = 300
                  ),
  sidebar,
  body)



server <- function(input, output) {

}

shinyApp(ui, server)
