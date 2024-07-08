library(stats)
library(dtw)
library(cluster)
library(dfoptim)

time_series <- read.csv("exploration/data/daily_return_database.csv")
time_series$Date <- as.Date(time_series$Date)

time_series[,1]

#smooth all of the columns
smoothed_series <- data.frame(Date = time_series$Date)
for (col in names(time_series)[2:ncol(time_series)]) {
  smoothed_series[[col]] <- ksmooth(
    x = time_series$Date, 
    y = as.numeric(time_series[[col]]), 
    kernel = "normal", 
    bandwidth = 100
  )$y
}

# plot a smoothed time series
plot(smoothed_series$Date, smoothed_series$AAPL_return, type = "l", xlab = "Time", ylab = "Daily Return", main = "Smoothed Time Series")
plot(time_series$Date, time_series$AAPL_return, type = "l", xlab = "Time", ylab = "Daily Return", main = "Original Time Series")
#do the above in a function
library(np)
smooth_series <- function(time_series, lmbda) {
  smoothed_series <- data.frame(Date = time_series$Date)
  for (col in names(time_series)[2:ncol(time_series)]) {
    smoothed_series[[col]] <- ksmooth(
      x = time_series$Date,
      y = as.numeric(time_series[[col]]),
      kernel = "normal",
      bandwidth = lmbda
    )$y
    # smoothed_series[[col]] <- npreg(as.numeric(time_series$Date),
    #  as.numeric(time_series[[col]]),bws=2418,bandwidth.compute=FALSE)
  }
  return(smoothed_series)
}


distance <- function(ts1, ts2) {
  return(dtw(ts1, ts2, keep = FALSE, distance.only = TRUE, window.type = "itakura")$distance)
}

distance <- function(ts1, ts2) {
  return(sum((ts1 - ts2)^2))
}

diss_matrix <- function(data) {
  n <- ncol(data)
  diss <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      diss[i, j] <- distance(data[i, ], data[j, ])
    }
  }
  return(diss)
}

kernel_ts_clustering <- function(time_series, lmbda, k) {
  smoothed_series <- smooth_series(time_series, lmbda)
  smoothed_series <- smoothed_series[-1, -1]
  diss <- diss_matrix(smoothed_series)
  cluster <- pam(diss, k = k)
  loss <- sum(cluster$objective)
  return(loss + lmbda)
}

objective_function <- function(lmbda) {
  if (lmbda <= 0) {
    return(Inf)
  }
  tryCatch({
    loss <- kernel_ts_clustering(time_series, lmbda, k = 3)
    return(loss)
  }, error = function(e) {
    return(Inf)
  })
}

result <- optimize(
  f = objective_function,
  interval = c(0.1, 100)  # Search interval for lmbda
)

result$minimum

lmbda = 10

k = 3
smoothed_series <- smooth_series(time_series, lmbda)
smoothed_series <- smoothed_series[-1, -1]
diss <- diss_matrix(smoothed_series)
cluster <- pam(diss, k = k)
clusters <- cluster$clustering

# use the clusters array to print all the time series with the same cluster
for (i in 1:k){
    plot(NULL, xlim = c(0, nrow(smoothed_series)), ylim = c(-0.25, 0.25), xlab = "Time", ylab = "Daily Return", main = "Cluster")
    for (j in 1:ncol(smoothed_series)){
        if (clusters[j] == i){
            lines(smoothed_series[,j+1], type = "l", col = i)
        }
    }
}

# use npreg
# simulate data of shorter length
# try to cluster the data
# after this use johns CRAN package, nonsmooth