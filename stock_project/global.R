# global.R
library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(plotly)
library(lubridate)
library(syuzhet)
library(xgboost)
stock_data <- read.csv("D:/R/stock_sentiment_analysis/stock_project/data/stock_data_preprocessed_HDB.csv", stringsAsFactors = FALSE)
sentiment_data <- read.csv("D:/R/stock_sentiment_analysis/stock_project/data/HSB_sentiment.csv", stringsAsFactors = FALSE)
stock_data$Date <- as.Date(stock_data$Date)
sentiment_data$date <- as.Date(sentiment_data$date)
sentiment_scores <- sentiment_data %>%
  mutate(score = get_sentiment(title, method = "syuzhet")) %>%
  group_by(date) %>%
  summarise(daily_sentiment = mean(score, na.rm = TRUE)) %>%
  ungroup()
merged_df <- stock_data %>%
  left_join(sentiment_scores, by = c("Date" = "date")) %>%
  mutate(daily_sentiment = ifelse(is.na(daily_sentiment), 0, daily_sentiment)) %>%
  arrange(Date) %>%
  mutate(target = lead(Close)) %>%
  na.omit()
features <- c("HDFCBANK.NS.Open", "HDFCBANK.NS.High", "HDFCBANK.NS.Low",
              "HDFCBANK.NS.Volume", "daily_sentiment")
n <- nrow(merged_df)
test_size <- 50
train_data <- merged_df[1:(n - test_size), ]
test_data <- merged_df[(n - test_size + 1):n, ]
X_train <- train_data[, features]
y_train <- train_data$Close
X_test <- test_data[, features]
y_test <- test_data$Close
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse"
)
model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100
)
test_preds <- predict(model, dtest)
test_mse <- mean((test_preds - y_test)^2)
test_rmse <- sqrt(test_mse)
importance <- xgb.importance(feature_names = features, model = model)
plot_df <- rbind(
  data.frame(
    Date = train_data$Date,
    Actual = train_data$Close,
    Predicted = NA,
    Type = "Training"
  ),
  data.frame(
    Date = test_data$Date,
    Actual = y_test,
    Predicted = test_preds,
    Type = "Testing"
  )
)
last_date <- max(merged_df$Date)
future_dates <- seq.Date(from = last_date + 1, by = "day", length.out = 90)
last_row <- tail(merged_df, 1)
future_df <- data.frame(Date = future_dates)
future_df$HDFCBANK.NS.Open <- jitter(rep(last_row$Close, 90), factor = 0.01)
future_df$HDFCBANK.NS.High <- future_df$HDFCBANK.NS.Open * runif(90, 1.005, 1.02)
future_df$HDFCBANK.NS.Low <- future_df$HDFCBANK.NS.Open * runif(90, 0.98, 0.995)
future_df$HDFCBANK.NS.Volume <- jitter(rep(last_row$HDFCBANK.NS.Volume, 90), factor = 0.1)
future_df$daily_sentiment <- jitter(rep(mean(tail(merged_df$daily_sentiment, 10)), 90), factor = 0.05)
X_future <- as.matrix(future_df[, features])
future_preds <- predict(model, X_future)
future_plot_df <- data.frame(
  Date = future_dates,
  Actual = NA,
  Predicted = future_preds,
  Type = "Future"
)
plot_df <- rbind(plot_df, future_plot_df)
model_results <- list(
  merged_df = merged_df,
  model = model,
  loss = test_mse,
  rmse = test_rmse,
  plot_df = plot_df,
  importance = importance,
  future_dates = future_dates,
  future_preds = future_preds
)