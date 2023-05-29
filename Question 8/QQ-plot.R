# Set designated seed 
set.seed(1501)

# Cauchy's distribuition parameters
location = -1.6
scale = 1

# Normal's distribuition parameters
mean = 3.6
sd = 3.4

# Generate sample
sample_size = 178
sample = rcauchy(sample_size, location, scale)

# Find theoretical quantiles
quantiles = (1:sample_size) / (sample_size + 1)
theoretical_quantiles_cauchy = qcauchy(quantiles, location, scale)
theoretical_quantiles_norm = qnorm(quantiles, mean, sd)

# sort sample
sample = sort(sample)
x = 1:sample_size

# Set a specific layout for plot
layout(matrix(c(1,2), ncol=2), widths=c(1,1), heights=c(1,1))

# Adjust the margins for first plot
par(mar=c(5, 4, 4, 0.5))

# Create a blank plot with the grid
plot(theoretical_quantiles_cauchy, sample, type = "n", xlab = "Quantis Teóricos (Cauchy)", ylab = "Valores da Amostra")
abline(h = pretty(range(sample)), v = pretty(range(theoretical_quantiles_cauchy)), col = "gray", lty = "dashed")

# Add the points and line to the plot
points(theoretical_quantiles_cauchy, sample)
qqline(sample, distribution = qcauchy, col="red",lwd = 1.5)

# Adjust the margins for second plot
par(mar=c(5, 0.5, 4, 4))

# Create a blank plot with the grid for the second plot
plot(theoretical_quantiles_norm, sample, type = "n", xlab = "Quantis Teóricos (Normal)", ylab = "", yaxt = "n")
abline(h = pretty(range(sample)), v = pretty(range(theoretical_quantiles_norm)), col = "gray", lty = "dashed")

# Add the points and line to the second plot
points(theoretical_quantiles_norm, sample)
qqline(sample, col="red",lwd = 1.5)


