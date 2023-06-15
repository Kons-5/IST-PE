set.seed(2904)

sample_size <- 3117
rate <- 9.5

sample <- rexp(sample_size, rate)   # Generate sample
s <- cumsum(sample)                 # Time at which each event occured
T <- ceiling(max(s))                # integer value >= time of last event
event_count <- table(floor(s))      # count the frequency of each value in the samples cumulative sum

# Find sample mean and distribuition expected value 
mean_counts <- mean(event_count)
expected_value <- rate

# Find absolute deviation
absolute_deviation <- abs(mean_counts - expected_value)
round(absolute_deviation, 4)

