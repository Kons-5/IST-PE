# Find the probability of the first digit being 1 or 6
prob39 <- log10(1 + 1/1) + log10(1 + 1/6)

# Define the range of exponents for the powers of 2
exponent_range <- 9:26

# Calculate the powers of 2
powers_of_two <- 2^exponent_range

# Convert to character strings to extract the first digit
first_digits <- substr(powers_of_two, 1, 1)

# Calculate the fraction of powers whose first digit is 3 or 9
fraction <- sum(first_digits %in% c("1", "6")) / length(powers_of_two)

abs_deviation <- abs(prob39 - fraction)
print(round(abs_deviation, 4))
