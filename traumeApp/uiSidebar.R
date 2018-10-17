sidebar <- dashboardSidebar(width = 250, #tilpase title tekst
  collapsed = TRUE, #skjule sidebar
  sidebarMenu(
    menuItem("Generell informasjon",
      tabName = "tab_info",
      icon = icon("info-circle")),

    menuItem("Filter data",
      tabName = "tab_filter",
      icon = icon("database")),

    tags$hr(), #linje

    menuItem("Ulykke",
      tabName = "tab_ulykke",
      icon = icon("bar-chart")),

    menuItem("Akutt",
      tabName = "tab_akutt",
      icon = icon("bar-chart")),

    menuItem("Intensiv",
      tabName = "tab_intensiv",
      icon = icon("bar-chart")),

    menuItem("Skadegradering",
      tabName = "tab_skadegrad",
      icon = icon("bar-chart")),

    tags$hr(), #linje

    menuItem("Virksomhetsrapport",
      tabName = "tab_virk",
      icon = icon("h-square")),

    menuItem("FAQ",
      tabName = "tab_manual",
      icon = icon("question-circle")),

    menuItem("Test",
      tabName = "tab_test",
      icon = icon("wrench"))
  )
)
