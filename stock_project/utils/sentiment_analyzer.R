# train_hdb_model.R

library(dplyr)
library(lubridate)
library(syuzhet)
library(xgboost)
library(zoo)

# Load data
stock <- read.csv("data/stock_data_preprocessed_HDB.csv", stringsAsFactors = FALSE)
stock$Date <- as.Date(stock$Date)

sentiment <- read.csv("data/HDB_sentiment.csv", stringsAsFactors = FALSE)
sentiment$date <- as.Date(sentiment$date)

# Sentiment score
sentiment_scores <- sentiment %>%
  mutate(score = get_sentiment(title, method = "syuzhet")) %>%
  group_by(date) %>%
  summarise(daily_sentiment = mean(score, na.rm = TRUE)) %>%
  ungroup()

# Merge and engineer features
df <- stock %>%
  left_join(sentiment_scores, by = c("Date" = "date")) %>%
  mutate(daily_sentiment = ifelse(is.na(daily_sentiment), 0, daily_sentiment)) %>%
  arrange(Date) %>%
  mutate(
    target = lead(Close),
    daily_sentiment_lag1 = lag(daily_sentiment, 1, default = 0),
    daily_sentiment_lag2 = lag(daily_sentiment, 2, default = 0),
    daily_sentiment_ma3 = rollmean(daily_sentiment, k = 3, fill = 0, align = "right"),
    price_momentum = Close / lag(Close, 5, default = Close[1]) - 1
  ) %>%
  na.omit()

# Select features and label
features <- c("Open", "High", "Low", "Volume", "daily_sentiment", 
              "daily_sentiment_lag1", "daily_sentiment_lag2", 
              "daily_sentiment_ma3", "Returns", "price_momentum")

X <- as.matrix(df[, features])
y <- df$target

# XGBoost training
params <- list(
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 5,
  subsample = 0.8,
  colsample_bytree = 0.8
)

model <- xgboost(data = X, label = y, params = params, nrounds = 100, verbose = 0)

# Save the model
if (!dir.exists("models")) dir.create("models")
model_path <- paste0("models/xgb_model_HDB_", format(Sys.Date(), "%Y%m%d"), ".rds")
saveRDS(model, model_path)

cat("✅ Model saved at:", model_path, "\n")
