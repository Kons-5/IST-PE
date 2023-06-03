set.seed(1274)

generate_geom <- function(p) {
  u <- runif(1)                          # Step 1: Generate a uniform random variable
  x <- floor(log(1 - u) / log(1 - p))  # Step 2: Apply the inverse CDF
  return(x)
}

p <- 0.2
n <- 1185
samples <- replicate(n, generate_geom(p))
sample_mean <- mean(samples)
sample_sd <- sd(samples)


sample_above_mean <- samples[samples > sample_mean]
proportion <- sum(sample_above_mean > (sample_mean + sample_sd)) / length(sample_above_mean)

print(round(proportion,4))