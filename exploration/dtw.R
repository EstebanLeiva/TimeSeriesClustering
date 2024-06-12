library(dtw)

# Load the csv file
stocks <- read.csv("exploration/data/adjusted_close_5stocks.csv")

alignment <- dtw(stocks$GSPC.Adjusted, stocks$DJI.Adjusted, keep = TRUE)
plot(alignment, type = "threeway")
alignment$distance

alignment <- dtw(stocks$GSPC.Adjusted, stocks$NYA.Adjusted, keep = TRUE)
plot(alignment, type = "threeway")
alignment$distance

alignment <- dtw(stocks$GSPC.Adjusted, stocks$GSPC.Adjusted, keep = TRUE)
plot(alignment, type = "threeway")
alignment$distance