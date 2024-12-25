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

riskMarginResultsServer <- function(id, risk_margin_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Risk Margin Download Table
    output$risk_margin_download_table <- renderDT({
        req(risk_margin_data())
        datatable(
        risk_margin_data() %>% mutate(Value = formatC(Value, format = "f", big.mark = ",", digits = 2)),
        options = list(pageLength = 5, scrollX = TRUE)
        )
    })
    
    # Download Handler for Risk Margin Results
    output$download_risk_margin <- downloadHandler(
        filename = function() {
        paste("risk_margin_results-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
        req(risk_margin_data())
        write_csv(risk_margin_data(), file)
        }
    )


  })
}