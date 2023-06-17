# Load required libraries - install.packages("pacman")
pacman::p_load(Rlab, tidyverse)

# Fix the random seed 
set.seed(1505)

# Declare both methods
method_1 <- function(sample_mean, sample_size){
  
  p = 0.8
  gamma <- 0.9
  z = qnorm((1 + gamma) / 2)
  
  # Apply quadratic formula
  a = 1 + z^2/sample_size
  b = -z^2/sample_size - 2 * sample_mean
  c = sample_mean^2
  
  lower <- (-b - sqrt(b^2 - 4*a*c))/(2*a)
  upper <- (-b + sqrt(b^2 - 4*a*c))/(2*a)
  
  interval_width = upper - lower
  
  # Return width for comparison
  return(interval_width)
}

method_2 <- function(sample_mean, sample_size){
  
  sd <- sqrt(sample_mean * (1 - sample_mean) / sample_size)
  gamma <- 0.9
  
  # Compute the 90% confidence interval
  z <- qnorm((1 + gamma) / 2)
  interval_width = 2*z*sd
  
  # Return width for comparison
  return(interval_width)
}

# Compute the differences
sample_size = c(30,50,100,200,300,500,1000)
mean_difference_vector = seq(1,7,by = 1)

for(i in 1:length(sample_size)){
  
  difference_vector = seq(1,3000,by=1)
  
  for(n in 1:3000){
    sample_vector = rbinom(n = sample_size[i], size = 1,prob = 0.8)
    
    sample_mean = mean(sample_vector)
    interval_width_1 = method_1(sample_mean, sample_size[i])
    interval_width_2 = method_2(sample_mean, sample_size[i])
    
    difference = interval_width_2 - interval_width_1
    difference_vector[n] = difference
  }
  
  mean_difference_vector[i] = mean(difference_vector)
}

# Create a data frame from the vectors
data <- data.frame(
  SampleSize = sample_size,
  MeanDifference = mean_difference_vector
)

# Plot the data in the final format
final_plot <- ggplot(data, aes(x = SampleSize, y = MeanDifference)) +
  ggtitle("Average Difference in Confidence Interval Widths Between Two Methods") +
  geom_line(color = "gray20", linetype = "dashed") +
  geom_hline(yintercept = 0, color = "blue", size = 0.5) +
  geom_segment(aes(xend = SampleSize, yend = 0)) +
  geom_point(shape = 19, size = 2) +
  scale_x_continuous(breaks = unique(sample_size)) +
  scale_y_continuous(limits = c(0, 0.0035), breaks = seq(0, 0.0035, by = 0.00025)) +
  xlab("Sample Size") + ylab("Average Difference Between Methods") +
  theme_linedraw(base_size = 8) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

# Display the resulting plot
# png("img/question09.png", width = 1920, height = 1080, units = "px", res = 300)
print(final_plot)
# dev.off()