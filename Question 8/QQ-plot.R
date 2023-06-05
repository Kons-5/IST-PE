# Set designated seed 
set.seed(1338)

# Cauchy's distribuition parameters
location = 2.2
scale = 1.6

# Normal's distribuition parameters
mean = 3.9
sd = 1.4

# Generate sample
sample_size = 124
sample = rcauchy(sample_size, location, scale)

# Find theoretical quantiles
quantiles = (1:sample_size) / (sample_size + 1)
theoretical_quantiles_cauchy = qcauchy(quantiles, location, scale)
theoretical_quantiles_norm = qnorm(quantiles, mean, sd)

# sort sample
sample = sort(sample)

# Create a blank plot with the grid
plot(theoretical_quantiles_cauchy, sample, type = "n", 
     xlab = "Theoretical Quantiles", 
     ylab = "Sample Values")
abline(h = pretty(range(sample)), v = pretty(range(theoretical_quantiles_cauchy)), 
       col = "gray", lty = "dashed")

# Add the points and line to the plot for Cauchy distribution
points(theoretical_quantiles_cauchy, sample, col = "red")
qqline(sample, distribution = qcauchy, col="red", lwd = 1.5)

# Overlay the points and line for Normal distribution
points(theoretical_quantiles_norm, sample, col = "blue")
qqline(sample, distribution = qnorm, col="blue", lwd = 1.5)

# Add legend
legend("topright", 
       legend = c("Cauchy", "Normal"), 
       col = c("red", "blue"), 
       lty = 1)
