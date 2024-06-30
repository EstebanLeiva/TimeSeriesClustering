library(quantmod)

sp500 <- new.env()
getSymbols("^GSPC", env = sp500, src = "yahoo",
           from = as.Date("2015-01-01"), to = as.Date("2024-01-01"))
gspc <- sp500$GSPC

dowjones <- new.env()
getSymbols("^DJI", env = dowjones, src = "yahoo",
           from = as.Date("2015-01-01"), to = as.Date("2024-01-01"))
dji <- dowjones$DJI

nyse_composite <- new.env()
getSymbols("^NYA", env = nyse_composite, src = "yahoo",
           from = as.Date("2015-01-01"), to = as.Date("2024-01-01"))
nya <- nyse_composite$NYA

wilshire5000 <- new.env()
getSymbols("^W5000", env = wilshire5000, src = "yahoo",
           from = as.Date("2015-01-01"), to = as.Date("2024-01-01"))
w5000 <- wilshire5000$W5000

russell2000 <- new.env()
getSymbols("^RUT", env = russell2000, src = "yahoo",
           from = as.Date("2015-01-01"), to = as.Date("2024-01-01"))
rut <- russell2000$RUT

#download tesla
tesla <- new.env()
getSymbols("TSLA", env = tesla, src = "yahoo",
           from = as.Date("2015-01-01"), to = as.Date("2024-01-01"))

#download rivian
rivian <- new.env()
getSymbols("RIVN", env = rivian, src = "yahoo",
           from = as.Date("2015-01-01"), to = as.Date("2024-01-01"))

#download amazon
amazon <- new.env()
getSymbols("AMZN", env = amazon, src = "yahoo",
           from = as.Date("2015-01-01"), to = as.Date("2024-01-01"))

#download apple
apple <- new.env()
getSymbols("AAPL", env = apple, src = "yahoo",
           from = as.Date("2015-01-01"), to = as.Date("2024-01-01"))

#download google
google <- new.env()
getSymbols("GOOGL", env = google, src = "yahoo",
           from = as.Date("2015-01-01"), to = as.Date("2024-01-01"))

stocks <- data.frame(
  Date = index(gspc),
  GSPC = Ad(gspc),
  DJI = Ad(dji),
  NYA = Ad(nya),
  W5000 = Ad(w5000),
  RUT = Ad(rut)
)


#get daily return data
stocks$GSPC_return <- c(NA, diff(log(stocks$GSPC)))
stocks$DJI_return <- c(NA, diff(log(stocks$DJI)))
stocks$NYA_return <- c(NA, diff(log(stocks$NYA)))
stocks$W5000_return <- c(NA, diff(log(stocks$W5000)))
stocks$RUT_return <- c(NA, diff(log(stocks$RUT)))

#delete the non return columns
stocks <- stocks[, -c(2:6)]
#delete the first row
stocks <- stocks[-1, ]

get_daily_returns <- function(symbols, start_date, end_date) {
  stock_env <- new.env()
  for (symbol in symbols) {
    getSymbols(symbol, env = stock_env, src = "yahoo",
               from = as.Date(start_date), to = as.Date(end_date))
  }
  stocks <- data.frame(Date = index(stock_env[[symbols[1]]]))

  for (symbol in symbols) {
    stocks[[symbol]] <- Ad(stock_env[[symbol]])
  }
  for (symbol in symbols) {
    return_col <- paste0(symbol, "_return")
    stocks[[return_col]] <- c(NA, diff(log(stocks[[symbol]])))
  }
  stocks <- stocks[-1, ]
  return(stocks)
}

#get a list of 100 company symbols
symbols <- c(
  "AAPL", "MSFT", "AMZN", "GOOG", "TSLA", "JNJ", "V", "WMT",
  "PG", "JPM", "UNH", "MA", "NVDA", "HD", "DIS", "PYPL", "NFLX", "PFE"
)

stocks <- get_daily_returns(symbols, "2015-01-01", "2024-01-01")

head(stocks)
#save the data
write.csv(stocks, "stocks.csv", row.names = FALSE)

#plot each of the returns in the same plot
plot(stocks$Date, stocks$GSPC_return, type = "l", col = "red")
lines(stocks$Date, stocks$DJI_return, col = "blue")
lines(stocks$Date, stocks$NYA_return, col = "green")
lines(stocks$Date, stocks$W5000_return, col = "black")
lines(stocks$Date, stocks$RUT_return, col = "purple")
