# app.R
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

source("global.R")

ui <- dashboardPage(
  dashboardHeader(title = "HDB Stock Sentiment"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Prediction", tabName = "prediction", icon = icon("chart-area"))
    )
  ),

  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),

    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          infoBoxOutput("price_box"),
          infoBoxOutput("sentiment_box"),
          infoBoxOutput("volume_box")
        ),
        fluidRow(
          box(title = "Stock Price", width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("price_plot", height = 300))
        ),
        fluidRow(
          box(title = "Sentiment Trend", width = 6, status = "info", solidHeader = TRUE,
              plotlyOutput("sentiment_trend", height = 250)),
          box(title = "Top News Headlines", width = 6, status = "warning", solidHeader = TRUE,
              DTOutput("news_table"))
        ),
        fluidRow(
          box(title = "Sentiment Score Distribution", width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("sentiment_dist", height = 250))
        )
      ),

      tabItem(
        tabName = "prediction",
        fluidRow(
          box(title = "Predicted vs Actual", width = 12, status = "primary", solidHeader = TRUE,
              plotlyOutput("prediction_plot", height = 300))
        ),
        fluidRow(
          box(title = "Model Performance", width = 6, status = "info", solidHeader = TRUE,
              verbatimTextOutput("model_metrics")),
          box(title = "Feature Importance", width = 6, status = "warning", solidHeader = TRUE,
              plotlyOutput("feature_importance"))
        )
      )
    )
  )
)

server <- function(input, output, session) {

  latest <- tail(stock_data, 1)
  previous <- tail(stock_data, 2)[1,]
  change <- (latest$Close - previous$Close) / previous$Close * 100
  color <- ifelse(change >= 0, "green", "red")

  output$price_box <- renderInfoBox({
    infoBox("Current Price",
            paste0("$", format(latest$Close, big.mark = ",", digits = 2)),
            paste0(ifelse(change >= 0, "▲", "▼"), " ", round(abs(change), 2), "%"),
            icon = icon("dollar-sign"), color = color)
  })

  output$sentiment_box <- renderInfoBox({
    avg_sentiment <- mean(model_results$merged_df$daily_sentiment, na.rm = TRUE)
    infoBox("Avg. Sentiment", round(avg_sentiment, 3),
            ifelse(avg_sentiment >= 0, "Positive", "Negative"),
            icon = icon("smile"), color = ifelse(avg_sentiment >= 0, "green", "red"))
  })

  output$volume_box <- renderInfoBox({
    latest_with_volume <- tail(stock_data[!is.na(stock_data$HDFCBANK.NS.Volume) &
                                            stock_data$HDFCBANK.NS.Volume != "NULL" &
                                            stock_data$HDFCBANK.NS.Volume > 0, ], 1)

    if(nrow(latest_with_volume) > 0) {
      infoBox("Latest Volume",
              format(latest_with_volume$HDFCBANK.NS.Volume, big.mark = ","),
              paste0(latest_with_volume$Date),
              icon = icon("chart-bar"), color = "blue")
    } else {
      infoBox("Trading Volume",
              "Data Pending",
              "Updated Daily",
              icon = icon("chart-bar"), color = "blue")
    }
  })

  output$price_plot <- renderPlotly({
    plot_ly(stock_data, x = ~Date, type = "candlestick",
            open = ~HDFCBANK.NS.Open, close = ~Close, high = ~HDFCBANK.NS.High,
            low = ~HDFCBANK.NS.Low, name = "HDB") %>%
      layout(title = "HDB Stock Price", xaxis = list(title = "Date"),
             yaxis = list(title = "Price (USD)"))
  })

  output$sentiment_trend <- renderPlotly({
    plot_ly(model_results$merged_df, x = ~Date, y = ~daily_sentiment, type = "scatter", mode = "lines",
            line = list(color = "blue")) %>%
      layout(title = "Daily Sentiment Trend", xaxis = list(title = "Date"),
             yaxis = list(title = "Sentiment Score"))
  })

  output$news_table <- renderDT({
    datatable(sentiment_data %>% arrange(desc(date)) %>% head(10) %>%
                select(date, title, ticker),
              options = list(dom = 't', pageLength = 5),
              colnames = c("Date", "Headline", "Ticker"))
  })

  output$sentiment_dist <- renderPlotly({
    plot_ly(model_results$merged_df, x = ~daily_sentiment, type = "histogram",
            marker = list(color = "skyblue")) %>%
      layout(title = "Sentiment Score Distribution",
             xaxis = list(title = "Sentiment Score"),
             yaxis = list(title = "Frequency"))
  })

  output$prediction_plot <- renderPlotly({
    df <- model_results$plot_df

    training_data <- df[df$Type == "Training", ]
    testing_data <- df[df$Type == "Testing", ]
    future_data <- df[df$Type == "Future", ]

    p <- plot_ly() %>%
      add_trace(data = training_data, x = ~Date, y = ~Actual,
                type = "scatter", mode = "lines",
                name = "Training (Actual)",
                line = list(color = "black", width = 1)) %>%

      add_trace(data = testing_data, x = ~Date, y = ~Actual,
                type = "scatter", mode = "lines",
                name = "Testing (Actual)",
                line = list(color = "black", width = 2)) %>%

      add_trace(data = testing_data, x = ~Date, y = ~Predicted,
                type = "scatter", mode = "lines",
                name = "Testing (Predicted)",
                line = list(color = "red", width = 2, dash = "dash")) %>%

      add_trace(data = future_data, x = ~Date, y = ~Predicted,
                type = "scatter", mode = "lines",
                name = "Future (Predicted)",
                line = list(color = "blue", width = 3)) %>%

      layout(title = "HDB Price Prediction",
             xaxis = list(title = "Date"),
             yaxis = list(title = "Price (USD)"),
             legend = list(x = 0.1, y = 0.9),
             shapes = list(
               list(type = "line",
                    x0 = min(testing_data$Date), x1 = min(testing_data$Date),
                    y0 = 0, y1 = 1, yref = "paper",
                    line = list(color = "gray", width = 1, dash = "dash"),
                    name = "Train/Test Split"),
               list(type = "line",
                    x0 = min(future_data$Date), x1 = min(future_data$Date),
                    y0 = 0, y1 = 1, yref = "paper",
                    line = list(color = "gray", width = 1, dash = "dash"),
                    name = "Present Day")
             ))

    return(p)
  })

  output$model_metrics <- renderPrint({
    cat("Model Performance Metrics:\n")
    cat("==========================\n")
    cat("Mean Squared Error (MSE):", round(model_results$loss, 4), "\n")
    cat("Root Mean Squared Error (RMSE):", round(model_results$rmse, 4), "\n")
    cat("\nTest Period: ", format(min(model_results$plot_df$Date[model_results$plot_df$Type == "Testing"]), "%Y-%m-%d"),
        " to ", format(max(model_results$plot_df$Date[model_results$plot_df$Type == "Testing"]), "%Y-%m-%d"), "\n", sep="")
    cat("\nFuture Prediction Period: ", format(min(model_results$future_dates), "%Y-%m-%d"),
        " to ", format(max(model_results$future_dates), "%Y-%m-%d"), "\n", sep="")
  })

  output$feature_importance <- renderPlotly({
    df <- model_results$importance
    plot_ly(df, x = ~Gain, y = ~Feature, type = "bar", orientation = "h",
            marker = list(color = "steelblue")) %>%
      layout(title = "Feature Importance",
             xaxis = list(title = "Gain"),
             yaxis = list(title = ""),
             margin = list(l = 120))
  })
}

shinyApp(ui = ui, server = server)