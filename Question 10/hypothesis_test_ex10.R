# Fix the random seed for reproducibility
set.seed(717)

# Define the true mean, variance, and sample size
mu <- 51.9
sigma <- sqrt(4)  # standard deviation is the square root of variance
n <- 20

# Generate m samples of size n from the Normal distribution
m <- 100
samples <- replicate(m, rnorm(n, mu, sigma))

# Conduct a t-test on each sample
p_values <- apply(samples, 2, function(x) t.test(x, mu = 50.9)$p.value)

# Estimate the probability of not rejecting the null hypothesis
alpha <- 0.03
probability <- mean(p_values > alpha)

# Print the estimated probability
print(round(probability, 3))