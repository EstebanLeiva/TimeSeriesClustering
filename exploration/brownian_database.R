set.seed(123)

brownian_motion <- function(n, mu, sigma) {
  dt <- 1 / n
  increments <- rnorm(n, mean = mu * dt, sd = sigma * sqrt(dt))
  path <- cumsum(increments)
  return(path)
}

combine_brownian_motions <- function(bm1, bm2) {
  if (length(bm1) == 0) {
    return(bm2)
  } else {
    bm2 <- bm2 + bm1[length(bm1)]
    combined_bm <- c(bm1, bm2)
    return(combined_bm)
  }
}

generate_brownian_motions <- function(
    n, dist_tuple,
    num_per_class,
    change_points,
    output_file = NULL) {
  num_motions <- length(dist_tuple) * num_per_class
  dist_simulated <- c()
  for (i in 1:length(dist_tuple)) {
    for (j in 1:num_per_class) {
      dist <- c()
      for (k in 1:(length(change_points) + 1)) {
        dist <- c(
          dist,
          c(runif(
            1, dist_tuple[i][k][1] - 0.5,
            dist_tuple[i][k][1] + 0.5
          ), runif(
            1, dist_tuple[i][k][2] - 0.1,
            dist_tuple[i][k][2] + 0.1
          ))
        )
      }
      dist_simulated <- c(dist_simulated, dist)
    }
  }
  print("pass")
  bm_df <- data.frame(matrix(
    nrow = n * (change_points + 1),
    ncol = num_motions
  ))
  for (i in 1:num_motions) {
    bm <- c()
    for (j in 1:(length(change_points) + 1)) {
      bm2 <- brownian_motion(
        n, dist_simulated[i][j][1],
        dist_simulated[i][j][2]
      )
      bm <- combine_brownian_motions(bm, bm2)
    }
    print(bm)
    bm_df[, i] <- bm
  }
  colnames(bm_df) <- paste0("BM_", 1:num_motions)
  if (!is.null(output_file)) {
    write.csv(bm_df, file = output_file, row.names = FALSE)
  }
  return(bm_df)
}

generate_brownian_motions <- function(
    n, dist_tuple,
    num_per_class,
    change_points,
    output_file = NULL) {
  num_motions <- length(dist_tuple) * num_per_class
  dist_simulated <- list()

  for (i in 1:length(dist_tuple)) {
    for (j in 1:num_per_class) {
      dist <- list()
      for (k in 1:(change_points + 1)) {
        dist[[k]] <- c(
          runif(
            1, dist_tuple[[i]][[k]][[1]] - 0.5,
            dist_tuple[[i]][[k]][[1]] + 0.5
          ),
          runif(
            1, dist_tuple[[i]][[k]][[2]] - 0.1,
            dist_tuple[[i]][[k]][[2]] + 0.1
          )
        )
      }
      dist_simulated <- c(dist_simulated, list(dist))
    }
  }

  bm_df <- data.frame(matrix(
    nrow = n * (change_points + 1),
    ncol = num_motions
  ))

  for (i in 1:num_motions) {
    bm <- c()
    for (j in 1:(change_points + 1)) {
      bm2 <- brownian_motion(
        n, dist_simulated[[i]][[j]][[1]],
        dist_simulated[[i]][[j]][[2]]
      )
      bm <- combine_brownian_motions(bm, bm2)
    }
    bm_df[, i] <- bm
  }

  colnames(bm_df) <- paste0("BM_", 1:num_motions)

  if (!is.null(output_file)) {
    write.csv(bm_df, file = output_file, row.names = FALSE)
  }

  return(bm_df)
}

generate_daily_returns <- function(brownian_df, output_file = NULL) {
  daily_returns_df <- data.frame(matrix(nrow = nrow(brownian_df), ncol = ncol(brownian_df)))
  for (i in 1:ncol(brownian_df)) {
    daily_returns_df[, i] <- c(0, diff(brownian_df[, i]))
  }
  colnames(daily_returns_df) <- colnames(brownian_df)
  if (!is.null(output_file)) {
    write.csv(daily_returns_df, file = output_file, row.names = FALSE)
  }
  return(daily_returns_df)
}

### Generate brownian motions ###
n <- 10
dist_tuple <- list(
  list(c(1, 0.5), c(3, 0.5), c(2, 0.1)),
  list(c(4, 0.5), c(5, 0.5), c(-1, 0.1)),
  list(c(-1, 0.5), c(3, 0.5), c(0.5, 0.1))
)
num_per_class <- 3
change_points <- 2
bm_df <- generate_brownian_motions(
  n, dist_tuple, num_per_class, change_points, "brownian_database.csv"
)

plot(bm_df$BM_8, type = "l", col = "blue", main = "Brownian Motions", xlab = "Time", ylab = "Value")

### Generate daily returns ###
bm_returns <- generate_daily_returns(bm_df, "brownian_returns.csv")
