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

stocks <- data.frame(
  Date = index(gspc),
  GSPC = Ad(gspc),
  DJI = Ad(dji),
  NYA = Ad(nya),
  W5000 = Ad(w5000),
  RUT = Ad(rut)
)