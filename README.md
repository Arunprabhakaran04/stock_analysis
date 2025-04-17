# Stock Price Prediction using Sentimnet Analysis

A Shiny dashboard application for analyzing HDFC Bank (HDB) stock performance, market sentiment, and price prediction.

## Project Overview

This project provides a comprehensive analysis tool for HDFC Bank stock with the following features:
- Real-time stock price monitoring with technical indicators
- Market sentiment analysis based on news headlines
- Advanced price prediction using machine learning models
- Risk assessment and investment insights

## Project Structure

```
├── main_files/
│   ├── .RData                      # R workspace data
│   ├── .Rhistory                   # R command history
│   ├── HSB_sentiment.csv           # Sentiment data for HSB
│   ├── sentiment_analysis.Rmd      # R Markdown for sentiment analysis
│   ├── sentiment_scrap.R           # Web scraping script for news data
│   └── temp_scraping.R             # Temporary scraping utilities
│
├── stock_project/
│   ├── data/                       # Data storage directory
│   ├── models/                     # Trained model storage directory
│   ├── utils/                      # Utility functions and scripts
│   ├── app.R                       # Main Shiny application file
│   ├── global.R                    # Global variables and data loading
│   └── sentiment_analyzer.R        # Sentiment analysis implementation
│
├── .gitignore                      # Git ignore file
├── Merged_data_HSB.csv             # Combined stock and sentiment data
├── README.md                       # Project documentation
├── Stock_data_analysis.Rmd         # Stock data preprocessing and visualization
├── individual_anlayis.R            # Individual stock analysis script
└── stock_prediciton.ipynb          # Python notebook for stock prediction
```

## Features

### Market Dashboard
- **Real-time Metrics**: Current price, market sentiment, trading volume, and prediction accuracy
- **Stock Price History**: Interactive candlestick chart with 50-day and 200-day Simple Moving Averages
- **Sentiment Analysis**: Trending sentiment scores and distribution analysis
- **Latest Market News**: Filterable news headlines with sentiment scores

### Price Prediction
- **Customizable Forecasting**: Adjustable forecast periods and model selection
- **Model Performance**: Detailed metrics and validation visualizations
- **Feature Importance**: Analysis of factors influencing price predictions
- **Risk Analysis**: Investment risk scoring and confidence intervals

## Technology Stack

- **R Shiny**: Interactive web application framework
- **Machine Learning**: 
  - XGBoost for price prediction in R
  - Neural networks for prediction in Python
- **Data Processing**: dplyr, lubridate, and zoo for data manipulation
- **Visualization**: Plotly for interactive charts
- **Sentiment Analysis**: syuzhet package for text sentiment scoring
- **Web Scraping**: Selenium for gathering news headlines

## Getting Started

### Prerequisites
- R (version 4.0.0 or higher)
- Python 3.7+ (for neural network model)
- Required R packages:
  ```R
  install.packages(c("shiny", "shinydashboard", "dplyr", "DT", "plotly", 
                    "lubridate", "syuzhet", "xgboost", "zoo"))
  ```
- Required Python libraries (for neural network):
  ```python
  pip install pandas numpy matplotlib tensorflow scikit-learn
  ```

### Installation
1. Clone this repository
2. Update file paths in `stock_project/global.R` and `stock_project/sentiment_analyzer.R` to match your system
3. Run the Shiny application:
   ```R
   shiny::runApp("path/to/stock_project")
   ```

## Data Pipeline

1. **Data Collection**:
   - Stock data retrieved from Yahoo Finance API
   - News headlines scraped using `main_files/sentiment_scrap.R`

2. **Preprocessing**:
   - Stock data cleaned and processed using `Stock_data_analysis.Rmd`
   - Individual stock analysis performed with `individual_anlayis.R`

3. **Sentiment Analysis**:
   - News headlines analyzed in `main_files/sentiment_analysis.Rmd`
   - Sentiment processing implementation in `stock_project/sentiment_analyzer.R`
   - Sentiment scores merged with stock data in `Merged_data_HSB.csv`

4. **Model Training**:
   - XGBoost model trained in R using `stock_project/sentiment_analyzer.R`
   - Neural network model developed in Python using `stock_prediciton.ipynb`

5. **Application Deployment**:
   - Shiny dashboard implemented in `stock_project/app.R`
   - Global configuration and data loading in `stock_project/global.R`

## Usage Guide

### Market Dashboard
- Monitor current market conditions and sentiment
- Analyze historical price movements and technical indicators
- Track sentiment trends and their correlation with price movements

### Price Prediction
- Select forecast horizon and model type
- View predicted prices with confidence intervals
- Analyze model performance and feature importance

## File Descriptions

### R Scripts
- `stock_project/app.R`: Main Shiny dashboard application
- `stock_project/global.R`: Global variables, data loading, and initial model training
- `stock_project/sentiment_analyzer.R`: Implementation of sentiment analysis and XGBoost model
- `main_files/sentiment_scrap.R`: Web scraping for financial news headlines
- `individual_anlayis.R`: Script for analyzing individual stock performance

### R Markdown Files
- `main_files/sentiment_analysis.Rmd`: Analysis of sentiment from scraped news data
- `Stock_data_analysis.Rmd`: Preprocessing and visualization of stock data

### Python Notebooks
- `stock_prediciton.ipynb`: Neural network implementation for stock price prediction

### Data Files
- `main_files/HSB_sentiment.csv`: Raw sentiment data
- `Merged_data_HSB.csv`: Combined stock and sentiment data

## Future Enhancements

- Integration with additional data sources
- Advanced natural language processing for improved sentiment analysis
- Automated trading strategy recommendations
- Portfolio optimization based on risk/reward metrics
- Comparison analysis with sector and market benchmarks

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- Financial data provided by Yahoo Finance
- News data collected from public financial news websites
- Inspired by quantitative finance and sentiment analysis research