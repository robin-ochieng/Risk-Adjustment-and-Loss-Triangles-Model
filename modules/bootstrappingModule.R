# modules/incrTriModule.R

bootstrappingUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
        bs4Card(
            title = "Bootstrapping Analysis",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            fluidRow(
                column(
                    width = 4,
                    selectInput(ns("boot_statutory_class"), "Select Statutory Class:", choices = NULL)
                ),
                column(
                    width = 3,
                selectInput(ns("loss_year_boot"), "Select Loss Year:", choices = 2000:2050, selected = 2020)
                ),
                column(
                    width = 5,
                    selectInput(
                        ns("outlier_option"), 
                        "Select 'Gross_Paid' Preprocessing Method:", 
                        choices = c(
                            "Use Whole Data" = "whole", 
                            "Remove Outliers" = "remove", 
                            "Use Numeric Threshold" = "threshold"
                            ),
                        selected = "whole"   
                        ),

                # 2) Conditional panel shown only if user selects "Remove Outliers"
                conditionalPanel(
                condition = paste0("input['", ns("outlier_option"), "'] == 'remove'"),
                # Add numeric inputs for lower and upper quantile thresholds
                fluidRow(
                    column(
                        width = 6,
                        numericInput(
                            ns("lower_q"),
                            "Lower Quantile Threshold:",
                            value = 0.15,
                            min = 0,
                            max = 1,
                            step = 0.01
                        )
                    ),
                    column(
                        width = 6,
                        numericInput(
                            ns("upper_q"),
                            "Upper Quantile Threshold:",
                            value = 0.85,
                            min = 0,
                            max = 1,
                            step = 0.01
                        )
                    )
                )
                ),
                # 3) Conditional panel for using numeric threshold fences
                conditionalPanel(
                condition = paste0("input['", ns("outlier_option"), "'] == 'threshold'"),
                fluidRow(
                    column(
                    width = 6,
                    numericInput(
                        ns("lower_fence"),
                        "Lower Fence:",
                        value = -100000,
                        step = 100000
                    )
                    ),
                    column(
                    width = 6,
                    numericInput(
                        ns("upper_fence"),
                        "Upper Fence:",
                        value = 100000,
                        step = 100000
                    )
                    )
                )
                )
            )
            ),
            br(), br(),
            fluidRow(
            hr(),
            actionButton(ns("run_bootstrap"), "Run Bootstrapping Analysis", class = "btn btn-primary btn-primary-custom"),
                br(), br()
            )
        )
    ),
    fluidRow(
            valueBoxOutput(ns("valuebox_margin_75"), width = 6),
            valueBoxOutput(ns("valuebox_margin_sum"), width =6)
            ),
    fluidRow(
        bs4Card(
            title = "Bootstrap Summary Totals",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            DTOutput(ns("bootstrap_summary"))
         ),
        bs4Card(
            title = "Confidence Levels ByOrigin",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            DTOutput(ns("bootstrap_confidence"))
         ),
        #bs4Card(
        #    title = "Risk Margin Calculations",
        #     status = "primary",
        #    solidHeader = TRUE,
        #    collapsible = TRUE,
        #    width = 12,
        #    DTOutput(ns("risk_margin_table"))
        # ),
        bs4Card(
            title = "Risk Margin Calculations",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            plotOutput(ns("bootstrap_plot"))
         )
      )
    )
}

bootstrappingServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # Populate Statutory Class Choices (repeated)
    observeEvent(data(), {
        req(data())
        updateSelectInput(session, "boot_statutory_class", choices = unique(data()$Statutory_Class))
    })
    
    # Bootstrapping Results
    bootstrap_results <- eventReactive(input$run_bootstrap, {
        req(data())
        req(input$boot_statutory_class)
        
        # Filter data by selected class and chosen loss year
        filtered_data <- data() %>%
        filter(Statutory_Class == input$boot_statutory_class,
                year(Loss_Date) >= input$loss_year_boot)
        
        # Handle outliers based on user selection
        if (input$outlier_option == "remove") {
            Q1 <- quantile(filtered_data$Gross_Paid, probs = input$lower_q, na.rm = TRUE)
            Q3 <- quantile(filtered_data$Gross_Paid, probs = input$upper_q, na.rm = TRUE)
            IQR <- Q3 - Q1

            filtered_data <- filtered_data %>%
                filter(
                    Gross_Paid >= (Q1 - 1.5 * IQR) &
                     Gross_Paid <= (Q3 + 1.5 * IQR)
                )
        } else if (input$outlier_option == "threshold") {
        # Use numeric thresholds (Tukey's fences or user-defined fences)
        filtered_data <- filtered_data %>%
          filter(
            Gross_Paid >= input$lower_fence,
            Gross_Paid <= input$upper_fence
          )
      } 
      # If "whole", do nothing (use entire dataset)
        
        # Create cumulative triangle
        filtered_data <- filtered_data %>%
        mutate(
            Loss_Year = year(Loss_Date),
            Dev_period = year(Paid_Date) - year(Loss_Date)
        ) %>%
        group_by(Loss_Year, Dev_period) %>%
        summarise(Gross_Amount = sum(Gross_Paid, na.rm = TRUE), .groups = "drop")
        
        inc_tri <- as.triangle(filtered_data, origin = "Loss_Year", dev = "Dev_period", value = "Gross_Amount")
        cum_tri <- incr2cum(inc_tri, na.rm = TRUE)
        
        Boot_Method <- BootChainLadder(cum_tri, R = 999, process.distr = "od.pois")
        list(Boot_Method = Boot_Method, Cum_Tri = cum_tri)
    })
    
    # Reactive expression for risk margin data
    risk_margin_data <- reactive({
        req(bootstrap_results())
        
        # Extract Bootstrap Summary and Confidence Levels
        boot_summary <- summary(bootstrap_results()$Boot_Method)$Totals
        confidence_level <- quantile(bootstrap_results()$Boot_Method, c(0.75))
        confidence_df <- as.data.frame(confidence_level$ByOrigin)
        
        # Mean IBNR from Bootstrap Summary
        mean_ibnr <- boot_summary["Mean IBNR", "Totals"]
        
        # Total IBNR at 75% CI from Bootstrap Summary
        total_ibnr_75 <- boot_summary["Total IBNR 75%", "Totals"]
        
        # Sum of IBNR at 75% CI by Origin from Confidence Levels
        sum_ibnr_75 <- sum(confidence_df$`IBNR 75%`, na.rm = TRUE)
        
        # Calculate Risk Margins
        risk_margin_75_ci <- total_ibnr_75 - mean_ibnr
        risk_margin_sum <- sum_ibnr_75 - mean_ibnr
        
        # Create Data Frame for Display
        risk_margin_df <- data.frame(
        Description = c("Risk Margin @ 75% CI", "Risk Margin from Summing"),
        Value = c(risk_margin_75_ci, risk_margin_sum)
        )
        
        risk_margin_df
    })
    
    # Display Bootstrap Summary
    output$bootstrap_summary <- renderDT({
        req(bootstrap_results())
        boot_summary <- summary(bootstrap_results()$Boot_Method)$Totals
        datatable(
        boot_summary %>% mutate_all(~formatC(.x, format = "f", big.mark = ",", digits = 2)),
        options = list(pageLength = 10, scrollX = TRUE)
        )
    })
    
    # Display Confidence Levels
    output$bootstrap_confidence <- renderDT({
        req(bootstrap_results())
        confidence_level <- quantile(bootstrap_results()$Boot_Method, c(0.75))
        confidence_df <- as.data.frame(confidence_level$ByOrigin)
        datatable(
        confidence_df %>% mutate_all(~formatC(.x, format = "f", big.mark = ",", digits = 2)),
        options = list(pageLength = 10, scrollX = TRUE)
        )
    })
    
    # Display Risk Margin Table in Bootstrapping Results Tab
    output$risk_margin_table <- renderDT({
        req(risk_margin_data())
        datatable(
        risk_margin_data() %>% mutate(Value = formatC(Value, format = "f", big.mark = ",", digits = 2)),
        options = list(pageLength = 5, scrollX = TRUE)
        )
    })
    
    # Plot Bootstrap Results
    output$bootstrap_plot <- renderPlot({
        req(bootstrap_results())
        plot(bootstrap_results()$Boot_Method)
    })

    # In your server function, define the renderValueBox outputs for risk margin:
    output$valuebox_margin_75 <- renderValueBox({
    req(risk_margin_data())
        # Grab the numeric value for "Risk Margin @ 75% CI"
        margin_75 <- risk_margin_data() %>%
            dplyr::filter(.data$Description == "Risk Margin @ 75% CI") %>%
            dplyr::pull(.data$Value) %>%
            as.numeric()
        valueBox(
            value    = formatC(margin_75, format = "f", big.mark = ",", digits = 0),
            subtitle = "Risk Margin at 75% Confidence Interval",
            icon     = NULL,
            color    = "primary"
        )
    })

    output$valuebox_margin_sum <- renderValueBox({
    req(risk_margin_data())
        # Grab the numeric value for "Risk Margin from Summing"
        margin_sum <- risk_margin_data() %>%
            dplyr::filter(.data$Description == "Risk Margin from Summing") %>%
            dplyr::pull(.data$Value) %>%
            as.numeric()
        valueBox(
            value    = formatC(margin_sum, format = "f", big.mark = ",", digits = 0),
            subtitle = "Aggregate Risk Margin at 75% Quantile",
            icon     = NULL,
            color    = "primary"
        )
    })
    

     return(reactive({ risk_margin_data() }))

  })
}