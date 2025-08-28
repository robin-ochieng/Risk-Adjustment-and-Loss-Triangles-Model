# UI function for data overview
dataOverviewUI <- function(id) {
  ns <- NS(id)
  tagList(
      fluidRow(
        class = "top-panels align-items-stretch",
        column(6, class = "d-flex",
          div(
            class = "upload-container1 flex-fill",
            fileInput(ns("file"), 
              label = tags$span("Upload Claims Data as an Excel or CSV File", class = "upload-label"),
              accept = c(".xlsx", ".xls", ".csv")
            )
          )
        ),
        column(6, class = "d-flex",
          div(
            class = "upload-container instruction-card flex-fill",
            tags$div(class = "instruction-title", "How to Prepare Data before Upload"),
            tags$p(class = "instruction-subtitle", "Ensure the data format is either a CSV or Excel file."),
            tags$div(class = "instruction-section-label", "Dataset Variable Definitions"),
            tags$ul(class = "instruction-list",
              tags$li(class = "instruction-item",
                icon("tag", class = "instruction-icon"),
                tags$span(class = "instruction-text",
                  tags$strong("Claim_No"), tags$span(": "), "Unique identifier for each claim."
                )
              ),
              tags$li(class = "instruction-item",
                icon("calendar-day", class = "instruction-icon"),
                tags$span(class = "instruction-text",
                  tags$strong("Loss_Date"), tags$span(": "), "Date when the loss occurred."
                )
              ),
              tags$li(class = "instruction-item",
                icon("calendar-check", class = "instruction-icon"),
                tags$span(class = "instruction-text",
                  tags$strong("Paid_Date"), tags$span(": "), "Date when the claim payment was made."
                )
              ),
              tags$li(class = "instruction-item",
                icon("money-bill-wave", class = "instruction-icon"),
                tags$span(class = "instruction-text",
                  tags$strong("Gross_Paid"), tags$span(": "), "Total amount paid for the claim (KES)."
                )
              ),
              tags$li(class = "instruction-item",
                icon("layer-group", class = "instruction-icon"),
                tags$span(class = "instruction-text",
                  tags$strong("Statutory_Class"), tags$span(": "), "The Class of Business."
                )
              )
            )
          )
        )
      ),
      br(),
      bs4Card(
        title = "Data Overview",
        status = "white",
        solidHeader = TRUE,
  class = "data-overview-card",
        width = 12,
        DTOutput(ns("data_table"))
      )
  )
}



# Server function for data overview
dataOverviewServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Reactive value for storing the uploaded data with validation
  # Reactive data loading
    data <- reactive({
      req(input$file)
      inFile <- input$file

    withProgress(message = 'Reading and validating data...', {
      setProgress(0.2)

      # Attempt to read data
      file_extension <- tools::file_ext(inFile$name)
      tryCatch({
      if (file_extension %in% c("xlsx", "xls")) {
        df <- readxl::read_excel(inFile$datapath) %>%
        mutate(
          Gross_Paid = as.numeric(Gross_Paid),
          Claim_No = as.character(Claim_No),
          Statutory_Class = as.character(Statutory_Class)
        )
      } else if (file_extension == "csv") {
        df <- read_csv(inFile$datapath, 
                       col_types = cols(
                         Claim_No = col_character(),
                         Paid_Date = col_character(),
                         Loss_Date = col_character(),
                         Gross_Paid = col_number(), 
                         Statutory_Class = col_character()))
      } else {
        stop("Unsupported file type. Please upload a CSV or Excel file.")
      }
        
        # Validate necessary columns
        requiredColumns <- c("Claim_No", "Paid_Date", "Loss_Date", "Gross_Paid", "Statutory_Class")
        if (!all(requiredColumns %in% names(df))) {
          stop("Data must contain the following columns: ", paste(requiredColumns, collapse=", "))
        }

        # Parse dates using lubridate to allow multiple formats
        # orders = c("dmy", "ymd", "mdy") means it will try day-month-year, then year-month-day, then month-day-year
        df <- df %>%
            mutate(
            Paid_Date = lubridate::parse_date_time(Paid_Date, orders = c("dmy", "ymd", "mdy")),
            Loss_Date  = lubridate::parse_date_time(Loss_Date,  orders = c("dmy", "ymd", "mdy"))
            )

        # Check if any of the date columns failed to parse
        if (any(is.na(df$Paid_Date)) || any(is.na(df$Loss_Date))) {
            warning("Some date values could not be parsed. Please ensure dates are in a recognized format.")
        }

        setProgress(1)
        return(df) 

      }, error = function(e) {
        # Handle errors in data format
        showModal(modalDialog(
          title = "Error in data format",
          paste("Please check your CSV file for the correct columns and data formats. Details: ", e$message),
          easyClose = TRUE,
          footer = NULL
        ))
        return(NULL)
      })
    })
  })
    

    output$data_table <- renderDT({
      req(data())
      datatable(data(),
                options = list(scrollX = TRUE, 
                              pageLength = 20,
                              autoWidth = TRUE,
                              paging = TRUE,
                              searching = FALSE,
                              info = FALSE,
                              initComplete = JS(
                                "function(settings, json) {",
                                "  $(this.api().table().header()).css({",
                                "    'background-color': '#FFFFFF',", 
                                "    'color': '#000000'",  
                                "  });",
                                "}"
                              )))
    })
    
  return(data)

  })
}

