# Load libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(forecast)
library(ggplot2)

# Load your cleaned dataset
Council <- read.csv("C://Samoda//KDU//2nd year//3rd semester//Fundamentals of Data Mining//Assignment//Assignment 2//Association Rule Mining//Council_Spending_Cleaned.csv")

# Convert DATE_PAID to Date format and extract Year
Council$DATE_PAID <- as.Date(Council$DATE_PAID, format = "%d/%m/%Y")
Council$Year <- as.numeric(format(Council$DATE_PAID, "%Y"))

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Council Spending Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("table")),
      menuItem("Spending Trends", tabName = "trends", icon = icon("chart-line")),
      menuItem("Regression & Forecast", tabName = "modeling", icon = icon("project-diagram")),
      menuItem("Expenditure Summary", tabName = "summary", icon = icon("file-alt"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Dataset Preview", DTOutput("dataTable"), width = 12)
              )
      ),
      tabItem(tabName = "trends",
              fluidRow(
                selectInput("selectedService", "Choose Service Area:", choices = unique(Council$PURPOSE)),
                box(title = "Yearly Spending Trend", plotlyOutput("linePlot"), width = 6),
                box(title = "Spending Distribution by Year", plotlyOutput("barPlot"), width = 6)
              )
      ),
      tabItem(tabName = "modeling",
              fluidRow(
                selectInput("regService", "Select Service for Modeling:", choices = unique(Council$PURPOSE)),
                box(title = "Linear Regression Model", plotlyOutput("regressionPlot"), width = 6),
                box(title = "Forecast (Next 5 Years)", plotlyOutput("forecastPlot"), width = 6)
              )
      ),
      tabItem(tabName = "summary",
              fluidRow(
                box(title = "Summary", width = 12, solidHeader = TRUE, status = "info",
                    verbatimTextOutput("summaryStats"))
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  output$dataTable <- renderDT({
    datatable(Council)
  })
  
  output$linePlot <- renderPlotly({
    df <- Council %>% filter(PURPOSE == input$selectedService)
    plot_ly(df, x = ~Year, y = ~AMOUNT____, type = 'scatter', mode = 'lines+markers', color = I("blue")) %>%
      layout(title = paste("Spending Over Time:", input$selectedService),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Expenditure"))
  })
  
  output$barPlot <- renderPlotly({
    df <- Council %>%
      group_by(Year) %>%
      summarise(TotalSpending = sum(AMOUNT____, na.rm = TRUE))
    
    plot_ly(df, x = ~Year, y = ~TotalSpending, type = 'bar', color = I("darkgreen")) %>%
      layout(title = "Total Council Spending by Year",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Total Expenditure"))
  })
  
  output$summaryStats <- renderPrint({
    summary(Council$AMOUNT____)
  })
  
  output$regressionPlot <- renderPlotly({
    df <- Council %>% filter(PURPOSE == input$regService)
    if (nrow(df) >= 2) {
      model <- lm(AMOUNT____ ~ Year, data = df)
      df$Predicted <- predict(model)
      
      plot_ly(df, x = ~Year, y = ~AMOUNT____, type = 'scatter', mode = 'markers', name = 'Actual') %>%
        add_trace(y = ~Predicted, mode = 'lines', name = 'Fitted', line = list(color = 'red')) %>%
        layout(title = paste("Linear Regression for:", input$regService),
               xaxis = list(title = "Year"),
               yaxis = list(title = "Expenditure"))
    }
  })
  
  output$forecastPlot <- renderPlotly({
    df <- Council %>% filter(PURPOSE == input$regService) %>% arrange(Year)
    if (nrow(df) >= 3) {
      ts_data <- ts(df$AMOUNT____, start = min(df$Year), frequency = 1)
      model <- auto.arima(ts_data)
      forecasted <- forecast(model, h = 5)
      
      forecast_df <- data.frame(Year = seq(max(df$Year) + 1, max(df$Year) + 5),
                                Forecast = as.numeric(forecasted$mean))
      
      plot_ly() %>%
        add_trace(x = df$Year, y = df$AMOUNT____, type = 'scatter', mode = 'lines+markers', name = 'Historical') %>%
        add_trace(x = forecast_df$Year, y = forecast_df$Forecast, type = 'scatter', mode = 'lines+markers', name = 'Forecast', line = list(color = 'orange')) %>%
        layout(title = paste("Forecast for:", input$regService),
               xaxis = list(title = "Year"),
               yaxis = list(title = "Expenditure"))
    }
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)
