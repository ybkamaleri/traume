sidebar <- dashboardSidebar(width = 250, #tilpase title tekst
  collapsed = TRUE, #skjule sidebar
  sidebarMenu(
    menuItem("Generell informasjon",
      tabName = "tab_info",
      icon = icon("info-circle")),

    menuItem("Filter data",
      tabName = "tab_filter",
      icon = icon("database")),


    menuItem("Virksomhetsrapport",
      tabName = "tab_virk",
      icon = icon("h-square")),

    menuItem("Test",
      tabName = "tab_test",
      icon = icon("wrench"))
  )
)
