# Set the seed
set.seed(1276)

# Define m and n
m <- 2947
n <- 12

# Generate m samples each of size n from a standard normal distribution
samples <- replicate(m, rnorm(n))

# Calculate the sum of squares for each sample
sum_sq <- apply(samples^2, 2, sum)

# Calculate the 0.35 quantile for the sample sum of squares
sample_quantile <- quantile(sum_sq, 0.35, type = 2)

# Calculate the 0.35 quantile for the theoretical chi-square distribution with df=n
theoretical_quantile <- qchisq(0.35, df = n)

# Calculate the absolute difference between the sample and theoretical quantiles
abs_diff <- abs(sample_quantile - theoretical_quantile)

# Print the absolute difference 0.o
print(round(abs_diff, 4))