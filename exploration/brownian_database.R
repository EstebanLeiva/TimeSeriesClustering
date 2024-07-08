set.seed(123)

brownian_motion <- function(n, mu, sigma) {
  dt <- 1 / n
  increments <- rnorm(n, mean = mu * dt, sd = sigma * sqrt(dt))
  path <- cumsum(increments)
  return(path)
}

bm_path <- brownian_motion(100, 2, 0.5)
plot(bm_path, type = "l", col = "blue", main = "Brownian Motion", xlab = "Time", ylab = "Value")

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

# generate a vector of means and standard deviations with length 30 with 10 values between 0 and 1, 10 values between 4 and 5, and 10 values between -1 and 0
mus <- c(runif(10, 1.5, 2), runif(10, 4, 5), runif(10, -2, -1))
sigmas <- c(runif(10, 0.5, 1.5), runif(10, 0.5, 1.5), runif(10, 0.5, 1.5))

bm_df <- generate_brownian_motions(100, mus, sigmas, 30, "brownian_database.csv")

plot(bm_df$BM_1, type = "l", col = "blue", main = "Brownian Motions", xlab = "Time", ylab = "Value")
