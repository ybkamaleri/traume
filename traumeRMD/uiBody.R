body <- dashboardBody(
  ## custom css
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  tabItems(
    tabItem(tabName = "tab_info",
      fluidPage(
        tags$div(HTML('<div style="text-align: center; color: #455; font-size: 30px; font-family: serif;"> Oversikt informasjon for Nasjonalt traumeregister (NTR) </div>')),
        tags$br(),
        fluidRow(
          valueBoxOutput("o_traume"),
          valueBoxOutput("o_ais"),
          valueBoxOutput("o_dead")),
        fluidRow(
          box(width = 12,
            dygraphOutput("traume_dygraph"))
        )
      )),

    ## data filtrering
    tabItem(tabName = "tab_filter",
      fluidPage(
        fluidRow(
          filterUI("dataFilter")
        )
      )),

    ## Ulykke
    tabItem(tabName = "tab_ulykke",
      fluidPage(
        fluidRow(

        )
      )),

    ## Akutt
    tabItem(tabName = "tab_akutt",
      fluidPage(
        fluidRow(

        )
      )),

    ## Skadegradering
    tabItem(tabName = "tab_skadegrad",
      skadeUI("skade")
    ),

    tabItem(tabName = "tab_virk",
      fluidPage(
        fluidRow(
          virkDataUI("virkData"),
          virkPlotUI("virkPlot")
        )
      )),

    ## Rapport
    tabItem(tabName = "tab_rap",
      fluidRow(
        box(
          width = 4,
          height = 100,
          solidHeader = TRUE,
          status = "primary",
          title = "Valg sykehus:",
          uiOutput("hosp")
        ),

        box(
          width = 4,
          height = 100,
          solidHeader = TRUE,
          status = "primary",
          title = "Valg periode:",
          dateRangeInput(inputId = "dato_rapport",
            label = NULL,
            start = Sys.Date() - 360,
            end = Sys.Date(),
            separator = " til ",
            format = "dd.mm.yyyy",
            startview = "month",
            language = "no",
            weekstart = 1)
        ),

        box(
          width = 4,
          height = 100,
          align = "center", offset = 2,
          ## title = "Rapport",
          background = "light-blue",
          tags$div(HTML("<div style='color: #FFFFFF; font-size: 18px; text-align: center;'>Trykk knappen for å kompilere rapporten til en PDF fil</div>")),
          tags$br(),
          downloadButton("downloadReport", label = "Last ned", class = "butangDownload")
        )
      ),
      fluidRow(
        column(width = 10,
          includeMarkdown("rapport.md")
        )
      )

      ## rapportUI("rapport")
    ),

    tabItem(tabName = "tab_manual",
      htmlOutput("txtSide")
    ),

    tabItem(tabName = "tab_test",
      fluidPage(
        fluidRow(
          verbatimTextOutput("test")
        )
      ))
  )
)
