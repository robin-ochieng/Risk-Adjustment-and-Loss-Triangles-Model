# modules/incrTriModule.R

riskMarginResultsUI <- function(id) {
  ns <- NS(id)
    fluidRow(
        bs4Card(
            title = "Risk Margin Results",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            DTOutput(ns("risk_margin_download_table")),
            downloadButton(ns("download_risk_margin"), "Download Risk Margin Results", class = "btn btn-primary btn-primary-custom")
          )
        )
}

riskMarginResultsServer <- function(id, risk_margin_data, all_boot_results) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Risk Margin Download Table
    output$risk_margin_download_table <- renderDT({
        req(risk_margin_data())
        datatable(
        risk_margin_data() %>%
          mutate(Value = formatC(Value, format = "f", big.mark = ",", digits = 2)),
        options = list(pageLength = 5, scrollX = TRUE)
        )
    })
    
    # 2) Download Handler for the multi-sheet workbook
    output$download_risk_margin <- downloadHandler(
      filename = function() {
        # e.g. risk_margin_results-2024-12-24.xlsx
        paste0("risk_margin_results-", Sys.Date(), ".xlsx")
      },
      content = function(file) {
        # We will build an Excel workbook
        # Make sure to have `openxlsx` in your DESCRIPTION or library() calls
        # e.g. library(openxlsx)
        req(all_boot_results())

        wb <- openxlsx::createWorkbook()

        # all_boot_results() is the reactiveValues we stored in bootstrappingServer
        # Loop over each key in the reactiveValues
        keys <- names(all_boot_results())
        for (k in keys) {
          info <- all_boot_results()[[k]]
          
          # info$boot_summary
          # info$confidence_df
          # info$method_used
          # info$statutory_class
          
          # Create a worksheet name from that key or a short version
          # Worksheet names can’t exceed 31 chars, so let's do something shorter:
          sheet_name <- substr(k, 1, 31)
          openxlsx::addWorksheet(wb, sheetName = sheet_name)

          # 1) Write a small header row about method/stat class
          #    We'll put this in the first few rows
          openxlsx::writeData(wb, sheet = sheet_name, 
                              x = data.frame(
                                Statutory_Class = info$statutory_class,
                                Method_Used     = info$method_used
                              ), 
                              startRow = 1, colNames = TRUE)

          # 2) Write the bootstrap summary totals below it
          openxlsx::writeData(wb, sheet = sheet_name, 
                              x = info$boot_summary %>% as.data.frame(),
                              startRow = 4, rowNames = TRUE)
          
          # 3) Write the confidence levels table below that
          row_start_conf <- 4 + nrow(info$boot_summary) + 2
          openxlsx::writeData(wb, sheet = sheet_name, 
                              x = info$confidence_df, 
                              startRow = row_start_conf, 
                              colNames = TRUE)
        }
        
        # Finally, save the workbook
        openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)
      }
    )
  })
}