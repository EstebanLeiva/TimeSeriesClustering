# TODO:
# 1. Define a kernel function over a time series
# (start with an indicator function)
# 2. Define a distance function between two time series (MSM, DTW)
# 3. Define a clustering algorithm over time series (K-medoids, K-prototypes)
# 4. Cluster the time series data

library(stats)
library(dtw)
library(cluster)

# Data Preparation
x <- 1:100
y1 <- 100 * sin(x)
y2 <- 100 * sin(x + 0.1)
y3 <- 100 * sin(x + 0.15)
y4 <- x + 10
y5 <- x + 20
y6 <- x + 30
y7 <- -x + 10
y8 <- -x + 20
y9 <- -x + 30
y10 <- ifelse(x <= 50, x + 10, -x + 10)

time_series <- data.frame(x = x, y1 = y1, y2 = y2, y3 = y3,
                          y4 = y4, y5 = y5, y6 = y6,
                          y7 = y7, y8 = y8, y9 = y9, y10 = y10)

plot(x, y1, type = "l", col = "red")
lines(x, y2, col = "blue")
lines(x, y3, col = "green")
lines(x, y4, col = "black")
lines(x, y5, col = "purple")
lines(x, y6, col = "orange")
lines(x, y7, col = "brown")
lines(x, y8, col = "pink")
lines(x, y9, col = "gray")
lines(x, y10, col = "#ff005d")

time_series <- t(time_series)
time_series <- time_series[-1, ]
dim(time_series)

# Algorithm
indicator <- function(x, a, b) {
  return(ifelse(a <= x  && x <= b, 1, 0))
}

kernel <- function(ts, f, a, b) {
  indices <- seq_along(ts)
  modified_values <- sapply(indices, function(i) f(i, a, b))
  return(ts * modified_values)
}

distance <- function(ts1, ts2) {
  return(dtw(ts1, ts2, keep = FALSE, distance.only = TRUE)$distance)
}

diss_matrix <- function(data, f, a, b) {
  n <- nrow(data)
  diss <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      diss[i, j] <- distance(kernel(data[i, ], f, a, b),
                             kernel(data[j, ], f, a, b))
    }
  }
  return(diss)
}

diss <- diss_matrix(time_series, indicator, 1, 50)

cluster <- pam(diss, k = 3)
clusters <- cluster$clustering

clusters

# Visualization
plot(x, y1, type = "l", col = clusters[1])
lines(x, y2, col = clusters[2])
lines(x, y3, col = clusters[3])
lines(x, y4, col = clusters[4])
lines(x, y5, col = clusters[5])
lines(x, y6, col = clusters[6])
lines(x, y7, col = clusters[7])
lines(x, y8, col = clusters[8])
lines(x, y9, col = clusters[9])
lines(x, y10, col = clusters[10])
