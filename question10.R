# Fix the random seed for reproducibility
set.seed(717)

# Define the true mean, variance, and sample size
mu <- 51.9
sigma <- sqrt(4)  # standard deviation is the square root of variance
n <- 20

# Generate m samples of size n from the Normal distribution
m <- 100
samples <- replicate(m, rnorm(n, mu, sigma))

# Conduct a z-test on each sample
z.test <- function(x, mu = 0, sigma.x = 1){
  xbar <- mean(x)
  se <- sigma.x / sqrt(length(x))
  z <- (xbar - mu) / se
  p.value <- 2 * (1 - pnorm(abs(z)))  # Two-tailed test
  return(list(statistic = z, p.value = p.value))
}

p_values <- apply(samples, 2, function(x) z.test(x, 50.9, sigma)$p.value)

# Estimate the probability of not rejecting the null hypothesis
alpha <- 0.03
probability <- mean(p_values > alpha)

# Print the estimated probability
print(round(probability, 3))
