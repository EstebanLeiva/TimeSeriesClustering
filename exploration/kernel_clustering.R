library(stats)
library(dtw)
library(cluster)
library(np)

smooth_series <- function(time_series, bw, kernel) {
  smooth_series <- time_series
  for (col in names(time_series)[1:ncol(smooth_series)]) {
    X <- 1:nrow(time_series)
    Y <- time_series[[col]]
    smooth_series[[col]] <- ksmooth(X, Y,
      kernel = kernel,
      bandwidth = bw, n.points = length(X)
    )$y
  }
  return(smooth_series)
}

weighted_distance <- function(ts1, ts2, weight) {
  local_distance <- 0
  for (i in seq_along(ts1)) {
    local_distance <- local_distance + weight[i] * (ts1[i] - ts2[i])^2
  }
  return(local_distance)
}

get_normal_weights <- function(n, center, sigma) {
  x <- seq(1, n)
  weight <- dnorm(x, mean = center, sd = sigma)
  return(weight)
}

distance_dtw <- function(ts1, ts2) {
  return(
    dtw(ts1, ts2,
      keep = FALSE, distance.only = TRUE,
      window.type = "itakura"
    )$distance
  )
}

distance_euclidean <- function(ts1, ts2) {
  return(sum((ts1 - ts2)^2))
}

diss_matrix <- function(data, distance) {
  n <- ncol(data)
  diss <- matrix(0, n, n)
  if (distance == "weighted") {
    sd <- n / 4
    center <- n / 2
    weight <- get_normal_weights(nrow(data), center, sd)
  }
  for (i in 1:n) {
    for (j in 1:n) {
      if (distance == "euclidean") {
        diss[i, j] <- distance_euclidean(data[, i], data[, j])
      } else if (distance == "dtw") {
        diss[i, j] <- distance_dtw(data[, i], data[, j])
      } else if (distance == "weighted") {
        diss[i, j] <- weighted_distance(data[, i], data[, j], weight)
      } else {
        print("Invalid distance")
      }
    }
  }
  return(as.dist(diss))
}

kernel_clustering <- function(
    time_series, k, bw,
    kernel, distance) {
  smoothed_series <- smooth_series(time_series, bw, kernel)
  diss <- diss_matrix(smoothed_series, distance)
  cluster <- pam(diss, k)
  return(cluster)
}

kernel_clustering_time_window <- function(
    time_series, k, bw, kernel = "normal",
    distance = "euclidean", window_length = NULL, change_points = NULL) {
  if (!is.null(window_length)) {
    n <- nrow(time_series)
    boundaries <- seq(1, n + 1, by = window_length)
    clusters <- matrix(
      NA,
      nrow = length(boundaries) - 1,
      ncol = ncol(time_series)
    )
    for (i in 1:(length(boundaries) - 1)) {
      window <- time_series[boundaries[i]:boundaries[i + 1] - 1, ]
      cluster <- kernel_clustering(
        window, k, bw,
        kernel, distance
      )
      clusters[i, ] <- cluster$clustering
    }
    return(clusters)
  } else if (!is.null(change_points)) { # the change points must include the first and last index
    clusters <- matrix(
      NA,
      nrow = length(change_points) - 1,
      ncol = ncol(time_series)
    )
    for (i in 1:(length(change_points) - 1)) {
      window <- time_series[change_points[i]:change_points[i + 1], ]
      cluster <- kernel_clustering(
        window, k, bw,
        kernel, distance
      )
      clusters[i, ] <- cluster$clustering
    }
    return(clusters)
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
time.seq <- data.frame(X = seq(0, 100, by = 0.01))
reg.eval <- npreg(bws = bw, newdata = time.seq)
plot(time_series$BM_1, type = "l", col = "blue", main = "Brownian Motions", xlab = "Time", ylab = "Value")
lines(time.seq$X, reg.eval$mean, col = "red")
lines(reg$mean, col = "green")

time_series <- read.csv("exploration/data/combined_brownian_database.csv")
X <- 1:nrow(time_series)
Y <- time_series[["BM_11"]]
smoothed <- ksmooth(X, Y, kernel = "normal", bandwidth = 3, n.points = length(X))
plot(time_series$BM_11, type = "l", col = "blue", main = "Combined Brownian Motions", xlab = "Time", ylab = "Value")
lines(X, smoothed$y, col = "red")

### Clustering example ###
time_series <- read.csv("exploration/data/combined_brownian_database.csv")
cluster <- kernel_clustering(time_series, 3, 3, "normal", "dtw")

clusters <- cluster$clustering
clusters
for (i in 1:3) {
  plot(NULL, xlim = c(0, nrow(time_series)), ylim = c(-5, 5), xlab = "Time", ylab = "Daily Return", main = "Cluster")
  for (j in 1:ncol(time_series)) {
    if (clusters[j] == i) {
      lines(time_series[, j + 1], type = "l", col = i)
    }
  }
}

### Time window clustering example ###
time_series <- read.csv("exploration/data/brownian_database.csv")
window_length <- 20
k <- 3
bw <- 2
clusters <- kernel_clustering_time_window(
  time_series, k, bw,
  "normal", "euclidean", window_length
)
clusters[1, ]
clusters[2, ]
clusters[3, ]

clusters <- clusters[1, ]

plot(NULL, xlim = c(0, nrow(time_series)), ylim = c(-5, 5), xlab = "Time", ylab = "Daily Return", main = "Cluster")
for (i in 1:3) {
  for (j in 1:ncol(time_series)) {
    if (clusters[j] == i) {
      lines(time_series[, j], type = "l", col = i)
    }
  }
}

### Returns time windows example ###
time_series <- read.csv("exploration/data/brownian_returns.csv")
window_length <- 20
k <- 3
bw <- 2
clusters <- kernel_clustering_time_window(
  time_series, k, bw,
  "normal", "euclidean", window_length
)

clusters <- clusters[1, ]

for (i in 1:3) {
  plot(NULL, xlim = c(0, nrow(time_series)), ylim = c(-1, 1), xlab = "Time", ylab = "Daily Return", main = "Cluster")
  for (j in 1:ncol(time_series)) {
    if (clusters[j] == i) {
      lines(time_series[, j], type = "l", col = i)
    }
  }
  abline(h = 0, col = "black")
}

# apply the recent kernel
# epanechnikov kernel
