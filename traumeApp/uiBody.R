body <- dashboardBody(
  ## custom css
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  tabItems(
    tabItem(tabName = "tab_info",
      fluidPage(
        tags$h3(HTML(paste0('<center>', "Oversikt informasjon for hele Nasjonalt traumeregister databasen", '</center>' ))),
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

    tabItem(tabName = "tab_rap",
      rapportUI("rapport")
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
