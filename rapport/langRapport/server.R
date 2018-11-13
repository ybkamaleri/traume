library(shiny)
library(rmarkdown)
library(jsonlite)

# Define server logic required to pull and process data
shinyServer(function(input, output) {

  report <- reactiveValues(filepath = NULL) #This creates a short-term storage location for a filepath

  observeEvent(input$generate, {

        progress <- shiny::Progress$new()
        # Make sure it closes when we exit this reactive, even if there's an error
        on.exit(progress$close())
        progress$set(message = "Gathering data and building report.",
                     detail = "This may take a while. This window will disappear
                     when the report is ready.", value = 1)
        client_code <-
          (clients %>%
          filter(client_name == input$client %>% as.character()) %>%
          select(client_key) %>%
          unique())$client_key

        client <- dim_filter("customVarValue1", operator = "EXACT", client_code %>% as.character(), not = FALSE)

        ## construct filter objects
        fc <- filter_clause_ga4(list(client), operator = "AND")

        raw_GA_data <- #Goes and gets data based upon user choices
          [data processing]

        if(is.null(raw_GA_data))
          showNotification(paste("No data exists for this client/time
                                  period combination. Please select another client."),
                           type = "error",
                           duration = NULL,
                           closeButton = FALSE,
                           action = a(href = "javascript:location.reload();", "Reload page")
          )


        dataInput <- # data processing step
          raw_GA_data %>% processing...

        params <- list(data = dataInput,
                       client = input$client)

        tmp_file <- paste0(tempfile(), ".pdf") #Creating the temp where the .pdf is going to be stored

        render("report/GA_report.Rmd",
               output_format = "all",
               output_file = tmp_file,
               params = params,
               envir = new.env())

        report$filepath <- tmp_file #Assigning in the temp file where the .pdf is located to the reactive file created above

        })

  # Hide download button until report is generated
  output$reportbuilt <- reactive({
    return(!is.null(report$filepath))
  })
  outputOptions(output, 'reportbuilt', suspendWhenHidden= FALSE)

  #Download report
  output$download <- downloadHandler(

    # This function returns a string which tells the client
    # browser what name to use when saving the file.
    filename = function() {
      paste0(input$client, "_", Sys.Date(), ".pdf") %>%
        gsub(" ", "_", .)
    },

    # This function should write data to a file given to it by
    # the argument 'file'.
    content = function(file) {

      file.copy(report$filepath, file)

    }
  )

})
