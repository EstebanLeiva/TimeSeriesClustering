library(quantmod)

get_daily_returns <- function(symbols, start_date, end_date) {
  stock_env <- new.env()
  # Download stock data for each symbol
  for (symbol in symbols) {
    getSymbols(symbol, env = stock_env, src = "yahoo",
               from = as.Date(start_date), to = as.Date(end_date))
  }
  stocks <- data.frame(
  Date = index(stock_env[[symbols[1]]])
  )
  
  for (symbol in symbols) {
    stock_data <- data.frame(Date = index(stock_env[[symbol]]), Adjusted = Ad(stock_env[[symbol]]))
    stocks <- merge(stocks, stock_data, by = "Date", all = TRUE, suffixes = c("", paste0(".", symbol)))
    names(stocks)[ncol(stocks)] <- symbol
  }
  
  # Calculate daily returns
  for (symbol in symbols) {
    return_col <- paste0(symbol, "_return")
    stocks[[return_col]] <- c(NA, diff(log(stocks[[symbol]])))
  }
  
  # Remove the original price columns
  stocks <- stocks[, !names(stocks) %in% symbols]
  
  # Remove the first row with NA values
  stocks <- stocks[-1, ]
  
  return(stocks)
}

symbols <- c(
  "AAPL", "MSFT", "AMZN", "GOOG", "TSLA", "JNJ", "V", "WMT",
  "PG", "JPM", "UNH", "MA", "NVDA", "HD", "DIS", "NFLX", "PFE"
)

stocks <- get_daily_returns(symbols, "2015-01-01", "2024-01-01")
# remove rows with NA values
stocks <- na.omit(stocks)
# Save the daily return data to a CSV file
write.csv(stocks, "daily_return_database.csv", row.names = FALSE)
