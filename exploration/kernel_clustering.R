library(stats)
library(dtw)
library(cluster)
library(np)

smooth_series <- function(time_series, bw, kernel = "normal") {
  smooth_series <- time_series  
  for (col in names(time_series)[1:ncol(smooth_series)]) {
    X <- 1:nrow(time_series)
    Y <- time_series[[col]]
    smooth_series[[col]] <- ksmooth(X, Y, kernel = kernel, bandwidth = bw, n.points = length(X))$y
  }
  return(smooth_series)
}

distance_dtw <- function(ts1, ts2) {
  return(dtw(ts1, ts2, keep = FALSE, distance.only = TRUE, window.type = "itakura")$distance)
}

distance_euclidean <- function(ts1, ts2) {
  return(sum((ts1 - ts2)^2))
}

diss_matrix <- function(data, distance) {
  n <- ncol(data)
  diss <- matrix(0, n, n)
  for (i in 1:n) {
    for (j in 1:n) {
      if (distance == "euclidean"){
        diss[i, j] <- distance_euclidean(data[, i], data[, j])
      } else if (distance == "dtw"){
        diss[i, j] <- distance_dtw(data[, i], data[, j])
      } else {
         print("Invalid distance")
      }
    }
  }
  return(diss)
}

kernel_clustering <- function(time_series, k, bw, kernel = "normal", distance = "euclidean") {
  smoothed_series <- smooth_series(time_series, bw, kernel)
  diss <- diss_matrix(smoothed_series, distance)
  cluster <- pam(diss, k = k)
  return(cluster)
}

kernel_clustering_time_window <- function(time_series, k, bw, kernel = "normal", distance = "euclidean", window_length = NULL, change_points = NULL) {
  if (!is.null(window_length)){
    n <- nrow(time_series)
    boundaries <- seq(1, n, by = window_length)
    clusters <- c()
    for (i in 1:(length(boundaries) - 1)){
      window <- time_series[boundaries[i]:boundaries[i + 1], ]
      cluster <- kernel_clustering(smooth_window, k, bw, kernel, distance)
      clusters <- c(clusters, cluster$clustering)
    }
    return(clusters)
  } else if (!is.null(change_points)){
    clusters <- c()
    for (i in 1:(length(change_points) - 1)){
      window <- time_series[change_points[i]:change_points[i + 1], ]
      cluster <- kernel_clustering(smooth_window, k, bw, kernel, distance)
      clusters <- c(clusters, cluster$clustering)
    }
  } else {
    print("Window length or change points must be provided")
  }
}

### Smoothing example ###
time_series <- read.csv("exploration/data/brownian_database.csv")
X <- 1:nrow(time_series)
Y <- time_series[["BM_1"]]
bw <- npregbw(formula = Y ~ X)
reg <- npreg(bws = bw)
time.seq <- data.frame(X=seq(0,100,by=0.01))
reg.eval <- npreg(bws=bw, newdata=time.seq)
plot(time_series$BM_1, type = "l", col = "blue", main = "Brownian Motions", xlab = "Time", ylab = "Value")
lines(time.seq$X, reg.eval$mean, col = "red")
lines(reg$mean,col="green")

time_series <- read.csv("exploration/data/combined_brownian_database.csv")
X <- 1:nrow(time_series)
Y <- time_series[["BM_1"]]
smoothed <- ksmooth(X, Y, kernel = "normal", bandwidth = 2, n.points = length(X))
bw <- npregbw(formula = Y ~ X)
reg <- npreg(bws = bw)
time.seq <- data.frame(X=seq(0,100,by=0.01))
reg.eval <- npreg(bws=bw, newdata=time.seq)
plot(time_series$BM_1, type = "l", col = "blue", main = "Combined Brownian Motions", xlab = "Time", ylab = "Value")
lines(X, smoothed$y, col = "red")
lines(time.seq$X, reg.eval$mean, col = "red")

### Clustering example ###
cluster <- kernel_ts_clustering(time_series, 3)

clusters <- cluster$clustering
for (i in 1:3){
    plot(NULL, xlim = c(0, nrow(smoothed_series)), ylim = c(-5, 5), xlab = "Time", ylab = "Daily Return", main = "Cluster")
    for (j in 1:ncol(smoothed_series)){
        if (clusters[j] == i){
            lines(smoothed_series[,j+1], type = "l", col = i)
        }
    }
}

#dtw
#simulated stocks with increases and decreases, and get daily returns
#real stocks returns and get daily returns
#time windows in change points
