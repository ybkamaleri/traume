library(shiny)
library(googleAuthR)
library(dplyr)

# Define UI
shinyUI(fluidPage(
  # Application title
  titlePanel("TITLE"),

  sidebarLayout(
    sidebarPanel(
      selectInput("client",
                  label = h4("Client"),
                  choices = [my own data] %>% .$client_name %>% unique()),
      dateInput("start_date", label = h4("Start date"), value = "2017-06-30"),
      dateInput("end_date", label = h4("End date")),
      actionButton("generate", "Generate Report", icon = icon("file"), # This is the only button that shows up when the app is loaded
                    style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      br(),
      br(),
      conditionalPanel(condition = "output.reportbuilt", # This button appears after the report has been generated and is ready for download.
                       downloadButton("download", "Download Report",
                                      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
      ),

  # Show a plot of the generated distribution
    mainPanel()
  )
))



## download button vises bare når gererate rappot er klar
