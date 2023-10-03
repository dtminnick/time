
library("forecast")
library("shiny")
library("shinyBS")

ui <- fluidPage(
        
        titlePanel("Time Series Insights & Modeling Environment"),
        
        sidebarLayout(
                
                sidebarPanel(
                        
                        fileInput("file", 
                                  "Upload Source Data"),
                        
                        selectInput("method", 
                                    "Forecast Method",
                                    choices = c("ARIMA", "ETS")),
                        
                        selectInput("frequency",
                                    "Time Series Frequency",
                                    choices = c("Daily" = 7, "Weekly" = 52, "Monthly" = 12, "Quarterly" = 4)),
                        
                        bsTooltip("frequency",
                                  "The sampling frequency of the time series.",
                                  "right",
                                  trigger = "hover",
                                  options = list(container = "body")),
                        
                        numericInput("horizon", 
                                     "Forecast Horizon", 
                                     12),
                        
                        bsTooltip("horizon", "The number of observations per unit of time.",
                                  "right", trigger = "hover", options = list(container = "body")),
                        
                        br(),
                        
                        actionButton("forecast", 
                                     "Generate Forecast"),
                        
                        br(), br(), br(),
                        
                        downloadButton("download", 
                                       "Download Forecast")
                        
                ), # sidebarPanel
                
                mainPanel(
                        
                        plotOutput("plot"),
                        
                        verbatimTextOutput("summary")
                        
                ) # mainPanel
                
        ) # sidebarLayout
        
) # fluidPage

server <- function(input, output) {
        
        data <- reactive({
                
                req(input$file)
                
                read.csv(input$file$datapath, header = TRUE)
                
        })
        
        output$plot <- renderPlot({
                
                if (is.null(data())) return(NULL)
                
                ts_data <- ts(data()$value, start = c(2018, 1), frequency = input$horizon)
                
                method <- input$method
                
                if (method == "ARIMA") {
                        
                        fit <- auto.arima(ts_data)
                        
                        forecast_data <- forecast(fit, h = input$horizon)
                        
                } else {
                        
                        fit <- ets(ts_data)
                        
                        forecast_data <- forecast(fit, h = input$horizon)
                        
                }
                
                plot(forecast_data, main = paste("Forecast using", method))
                
                abline(v = length(ts_data), col = "red", lty = 2)
                
        })
        
        output$summary <- renderPrint({
                
                if (is.null(data())) return(NULL)
                
                ts_data <- ts(data()$value, start = c(2018, 1), frequency = input$horizon)
                
                method <- input$method
                
                if (method == "ARIMA") {
                        
                        fit <- auto.arima(ts_data)
                        
                        forecast_data <- forecast(fit, h = input$horizon)
                        
                } else {
                        
                        fit <- ets(ts_data)
                        
                        forecast_data <- forecast(fit, h = input$horizon)
                        
                }
                
                summary(forecast_data)
                
        })
        
        output$download <- downloadHandler(
                
                filename = function() {
                        
                        paste("forecast_", input$method, ".csv", sep = "")
                        
                },
                
                content = function(file) {
                        
                        if (is.null(data())) return(NULL)
                        
                        ts_data <- ts(data()$value, frequency = input$horizon)
                        
                        method <- input$method
                        
                        if (method == "ARIMA") {
                                
                                fit <- auto.arima(ts_data)
                                
                                forecast_data <- forecast(fit, h = input$horizon)
                                
                        } else {
                                
                                fit <- ets(ts_data)
                                
                                forecast_data <- forecast(fit, h = input$horizon)
                                
                        }
                        
                        write.csv(forecast_data, file)
                }
        )
}

shinyApp(ui, server)
