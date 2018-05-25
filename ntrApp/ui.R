
############################# Sidebar ###############################
sidebar <- dashboardSidebar(
  width = 250,
  sidebarMenu(id = "sidebar",
              menuItem("Generell informasjon", tabName = "info", icon = icon("info-circle")),
              menuItem("Ulykke og skadegradering", tabName = "skade", icon = icon("medkit")),
              conditionalPanel("input.sidebar === 'skade'",
                               selectInput(inputId = "ulykkeType", "Valg type ulykke/skade:",
                                           choices = list("Transport" = 1,
                                                          "Fall" = 2,
                                                          "Vold" = 3,
                                                          "Selvpåført" = 4,
                                                          "Arbeid" = 5,
                                                          "Sport og fritid" = 6,
                                                          "Brann og inhalasjon" = 7,
                                                          "Annen" = 8),
                                           selected = 1,
                                           width = '90%')),
              menuItem("Helse enheten", tabName = "unit", icon = icon("h-square"),
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

              menuItem("Sykehusopphod", tabName = "dag", icon = icon("bed"))
              ))


############################## Body ##################################

body <-  dashboardBody(
  ## custom css
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  ## css for valueBox
  ## tags$style(".small-box.bg-blue { background-color: #FFFF00 ; color: #000000; }"),

  tabItems(
    tabItem(tabName = "info",
            fluidRow(
              ## dynamisk output
              valueBoxOutput("ntrAntall"),
              valueBoxOutput("ntrAlvor"),
              valueBoxOutput("ntrDie")
            )),
    tabItem(tabName = "skade",
            fluidPage(
              tags$h3(textOutput("titleUT")),
              fluidRow(
                box(
                  width = 5,
                  ## title = "Kroppsregion",
                  selectInput(inputId = "kropp", "Valg en eller flere kroppsregion:",
                              choices = list("Head" = 1,
                                             "Face" = 2,
                                             "Neck" = 3,
                                             "Thorax" = 4,
                                             "Abdomen" = 5,
                                             "Upper extremity" = 7,
                                             "Lower extremity" = 8,
                                             "External and other" = 9),
                              selected = 1,
                              multiple = TRUE,
                              width = '99%')),
                box(
                  width = 3,
                  ## title = "Skadegradering",
                  checkboxGroupInput(inputId = "sgrad", "Valg skadegrader:",
                                     choices = list("1" = 1,
                                                    "2" = 2,
                                                    "3" = 3,
                                                    "4" = 4,
                                                    "5" = 5,
                                                    "6" = 6),
                                     inline = TRUE,
                                     selected = 2,
                                     )),
                ## Transport type
                uiOutput(outputId = "box")
              ),
              fluidRow(
                box(
                  width = 8,
                  DT::dataTableOutput("accTable")
                )
              )
            )
            ),
    tabItem(tabName = "dag",
            tags$h4("Sykehusopphold"))
  )
)



########################## Page ##################################
ui <- dashboardPage(

  dashboardHeader(title = "Nasjonalt traumeregister",
                  titleWidth = 250
                  ),
  sidebar,
  body)
