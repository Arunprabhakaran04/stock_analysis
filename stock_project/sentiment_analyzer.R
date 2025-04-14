library(dplyr)
library(lubridate)
library(syuzhet)
library(xgboost)
library(zoo)

stock_path <- "D:/R/stock_sentiment_analysis/stock_project/data/stock_data_preprocessed_HDB.csv"
sentiment_path <- "D:/R/stock_sentiment_analysis/stock_project/data/HSB_sentiment.csv"
model_dir <- "D:/R/stock_sentiment_analysis/stock_project/models"

stock <- read.csv(stock_path, stringsAsFactors = FALSE)
stock$Date <- as.Date(stock$Date)

sentiment <- read.csv(sentiment_path, stringsAsFactors = FALSE)
sentiment$date <- as.Date(sentiment$date)

sentiment_scores <- sentiment %>%
  mutate(score = get_sentiment(title, method = "syuzhet")) %>%
  group_by(date) %>%
  summarise(daily_sentiment = mean(score, na.rm = TRUE)) %>%
  ungroup()

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

features <- c(
  "HDFCBANK.NS.Open", "HDFCBANK.NS.High", "HDFCBANK.NS.Low", "HDFCBANK.NS.Volume",
  "daily_sentiment", "daily_sentiment_lag1", "daily_sentiment_lag2",
  "daily_sentiment_ma3", "daily_return", "price_momentum"
)


X <- as.matrix(df[, features])
y <- df$target

params <- list(
  objective = "reg:squarederror",
  eta = 0.1,
  max_depth = 5,
  subsample = 0.8,
  colsample_bytree = 0.8
)
features <- colnames(X) 

model <- xgboost(
  data = X,
  label = y,
  params = params,
  nrounds = 100,
  verbose = 0,
  feature_names = features  
)

if (!dir.exists(model_dir)) dir.create(model_dir, recursive = TRUE)
model_path <- file.path(model_dir, paste0("xgb_model_HDB_", format(Sys.Date(), "%Y%m%d"), ".rds"))
saveRDS(model, model_path)

cat("Model saved at:", model_path, "\n")

