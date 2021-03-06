body <- shinydashboard::dashboardBody(
  ## custom css
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),

  tabItems(
    tabItem(tabName = "tab_info",
      fluidPage(
        tags$div(HTML('<div style="text-align: center; color: #455; font-size: 30px; font-family: serif;"> Testversjon NTR-resultat </div>')),
        tags$div(HTML('<div style="text-align: center; color: #500; font-size: 15px; font-family: serif;">Ta kontakt med
<a href="http://nasjonalttraumeregister.no/" target="_blank"> Nasjonalt traumeregister </a> for tall fra ekte data </div>')),
tags$br(),
fluidRow(
  valueBoxOutput("o_traume"),
  valueBoxOutput("o_ais"),
  valueBoxOutput("o_dead")),
fluidRow(
  box(width = 12,
    dygraphs::dygraphOutput("traume_dygraph"))
),
column(width = 8,
  includeMarkdown("tips.md")
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
      ## virkDataUI("virkData"),
      ## virkPlotUI("virkPlot")
      virkUI("virksomhet")
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
