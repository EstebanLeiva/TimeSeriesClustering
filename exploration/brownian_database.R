set.seed(123)

brownian_motion <- function(n, mu, sigma) {
  dt <- 1 / n
  increments <- rnorm(n, mean = mu * dt, sd = sigma * sqrt(dt))
  path <- cumsum(increments)
  return(path)
}

combine_brownian_motions <- function(n, mu1, sigma1, mu2, sigma2) {
  if (n %% 2 != 0) {
    stop("n must be an even number")
  }
  bm1 <- brownian_motion(n/2, mu1, sigma1)
  bm2 <- brownian_motion(n/2, mu2, sigma2)
  bm2 <- bm2 + bm1[n/2]
  combined_bm <- c(bm1, bm2)
  return(combined_bm)
}

generate_brownian_motions <- function(n, mus, sigmas, num_motions, output_file = NULL) {
  bm_df <- data.frame(matrix(nrow = n, ncol = num_motions))
  for (i in 1:num_motions) {
    bm_df[, i] <- brownian_motion(n, mus[i], sigmas[i])
  }
  colnames(bm_df) <- paste0("BM_", 1:num_motions)
  if (!is.null(output_file)) {
    write.csv(bm_df, file = output_file, row.names = FALSE)
  }
  return(bm_df)
}

generate_combined_brownian_motions <- function(n, mus1, sigmas1, mus2, sigmas2, num_motions, output_file = NULL) {
  bm_df <- data.frame(matrix(nrow = n, ncol = num_motions))
  for (i in 1:num_motions) {
    bm_df[, i] <- combine_brownian_motions(n, mus1[i], sigmas1[i], mus2[i], sigmas2[i])
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

### PLOTS ###
#plot combined brownian motion
bm_combined <- combine_brownian_motions(100, 2, 0.5, -1, 0.5)
plot(bm_combined, type = "l", col = "blue", main = "Combined Brownian Motion", xlab = "Time", ylab = "Value")

#plot normal brownian motion
bm_path <- brownian_motion(100, 2, 0.5)
plot(bm_path, type = "l", col = "blue", main = "Brownian Motion", xlab = "Time", ylab = "Value")

### GENERATION OF DATASET ###

# Brownian Motions
mus <- c(runif(10, 1.5, 2), runif(10, 4, 5), runif(10, -2, -1))
sigmas <- c(runif(10, 0.5, 1.5), runif(10, 0.5, 1.5), runif(10, 0.5, 1.5))

bm_df <- generate_brownian_motions(30, mus, sigmas, 30, "brownian_database.csv")

plot(bm_df$BM_1, type = "l", col = "blue", main = "Brownian Motions", xlab = "Time", ylab = "Value")

# Combined Brownian Motions
mus1 <- c(runif(10, 1.5, 2), runif(10, 4, 5), runif(10, -2, -1))
sigmas1 <- c(runif(10, 0.5, 1), runif(10, 0.5, 1), runif(10, 0.5, 1))
mus2 <- c(runif(10, -1, -0.5), runif(10, 0.25, 0.5), runif(10, 0, 1))
sigmas2 <- c(runif(10, 0.5, 1), runif(10, 0.5, 1), runif(10, 0.5, 1))

bm_combined_df <- generate_combined_brownian_motions(30, mus1, sigmas1, mus2, sigmas2, 30, "combined_brownian_database.csv")

plot(bm_combined_df$BM_24, type = "l", col = "blue", main = "Combined Brownian Motions", xlab = "Time", ylab = "Value")

# Daily Returns
bm_returns <- generate_daily_returns(bm_combined_df, "browniancombined_returns.csv")

plot(bm_returns$BM_22, type = "l", col = "blue", main = "Daily Returns", xlab = "Time", ylab = "Value")
