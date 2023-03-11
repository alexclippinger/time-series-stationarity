
time_series_data <- function() {
  
  # Global ------------------------------------------------------------------

  library(shiny)
  library(plotly)
  library(ggplot2)
  
  start_date <- dateInput("start_date", "Start Date:", value = Sys.Date() - 365)
  end_date <- dateInput("end_date", "End Date:", value = Sys.Date())
  
  stationarity <- selectInput("stationarity", "Stationarity:", choices = c("Stationary", "Non-Stationary"), selected = "Stationary")
  nonstationary_type <- selectInput("nonstationary_type", "Non-Stationary Type", 
                                    choices = c("Unit Root", "Trend", "Varying Variance", "Seasonality"))
  
  shinyApp(
    
    # UI ----------------------------------------------------------------------
    
    ui <- fluidPage(
      tags$h3("Explore Stationarity"),
      tabPanel(
        "Time Series",
        
        # Sidebar -----------------------------------------------------------------
        sidebarPanel(
          width = 3,
          start_date,
          end_date,
          stationarity,
          conditionalPanel(
            condition = "input.stationarity == 'Non-Stationary'",
            nonstationary_type
          ),
          actionButton("regenerate", "Randomize Data")
        ),
        
        # Main Panel --------------------------------------------------------------
        mainPanel(
            
          # Visualizing Non-Stationarity ----------------------------------------
          tabPanel(
            "Visualizing Non-Stationarity",
            fluidRow(
              column(
                width = 12,
                br(),
                plotlyOutput("plot"),
                br(),
                plotlyOutput("acf_plot"),
              )
            )
          )
        ) # End of main panel
        
        # -------------------------------------------------------------------------
      )
    ),
    
    # Server ------------------------------------------------------------------
    
    server <- function(input, output, session) {
      
      data <- eventReactive(c(input$regenerate, input$stationarity, input$nonstationary_type), ignoreNULL = FALSE, {
        # Get sequence of dates
        seq_date <- seq(from = input$start_date, to = input$end_date, by = "day")
        n <- length(seq_date)
        
        if (input$stationarity == "Stationary") {
          ts_data <- data.frame(
            date = seq_date,
            value = rnorm(n, sd = 10)
          )
        } else if (input$stationarity == "Non-Stationary" & input$nonstationary_type == "Unit Root") {
          value <- arima.sim(model = list(order = c(1, 1, 0), ar = 0.8), n = n - 1)
          ts_data <- data.frame(
            date = seq_date,
            value = value
          )
          ts_data$value <- as.numeric(value)
        } else if (input$stationarity == "Non-Stationary" & input$nonstationary_type == "Trend") {
          trend <- seq_along(1:n)
          slope <- rnorm(1, mean = 0, sd = 0.2)
          randomness <- rnorm(n, mean = 0, sd = 0.2)
          ts_data <- data.frame(
            date = seq_date,
            value = slope * trend + sqrt(trend) * randomness
          )
        } else if (input$stationarity == "Non-Stationary" & input$nonstationary_type == "Varying Variance") {
          ts_data <- data.frame(
            date = seq_date,
            value = rnorm(n, mean = 0, sd = seq(1, 10, length.out = n))
          )
        } else if (input$stationarity == "Non-Stationary" & input$nonstationary_type == "Seasonality") {
          ts_data <- data.frame(
            date = seq_date,
            value = sin(2 * pi * (1:n) / (n / 4)) + rnorm(n, sd = 0.2)
          )
        } else {
          stop("Cannot create dataframe.")
        }
        
        ts_data
      })
      
      # Plots -------------------------------------------------------------------
      
      # Render plot
      output$plot <- renderPlotly({
        p <- ggplot(data(), aes(x = date, y = value)) +
          geom_line() +
          geom_point(size = 0.5) +
          labs(x = "Date", y = "Value",
               title = paste(input$stationarity, "Time-Series")) +
          theme_minimal()
        
        ggplotly(p)
      })
      
      output$acf_plot <- renderPlotly({
        
        x <- data()$value
        acf <- acf(x = x, plot = FALSE)
        acf_df <- with(acf, data.frame(lag, acf))
        conf.level <- 0.95
        ciline <- qnorm((1 - conf.level)/2)/sqrt(length(x))
        
        p <- ggplot(data = acf_df, mapping = aes(x = lag, y = acf)) +
          geom_hline(aes(yintercept = 0)) +
          geom_segment(mapping = aes(xend = lag, yend = 0)) +
          geom_hline(aes(yintercept = ciline), linetype = 2, color = 'darkblue') + 
          geom_hline(aes(yintercept = -ciline), linetype = 2, color = 'darkblue') +
          labs(x = "Lag", y = "ACF", title = "ACF Plot") +
          theme_minimal() 
      })
    },
    options = list(height = 1000)
  )
}