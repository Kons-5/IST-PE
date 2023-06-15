# Load required libraries
library(ggplot2, ggthemes)

# Set parameters
set.seed(1338)  # Fix the random seed for reproducibility
location <- 2.2 # Cauchy's distribution parameters
scale <- 1.6
mean <- 3.9     # Normal's distribution parameters
sd <- sqrt(1.4)

# Generate sample
sample_size <- 124
sample <- rcauchy(sample_size, location, scale)
sample <- sort(sample) # Sort sample

# Find theoretical quantiles
quantiles <- (1:sample_size) / (sample_size + 1)
theoretical_quantiles_cauchy <- qcauchy(quantiles, location, scale)
theoretical_quantiles_norm <- qnorm(quantiles, mean, sd)

# Create main plot
main_plot <- ggplot() + labs(title = "QQ-Plot: Exploring Deviation from Theoretical Distributions") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(aes(x = theoretical_quantiles_cauchy, y = sample, colour="Cauchy"), shape=1, alpha=0.5, size=1.5) +
  geom_point(aes(x = theoretical_quantiles_norm, y = sample, colour="Normal"), shape=1, alpha=0.5, size=1.5) +
  labs(x = "Theoretical Quantiles", y = "Sample Values", color = "Distributions") +
  scale_color_manual(values = c("Cauchy" = "blue", "Normal" = "red")) +
  theme_linedraw(base_size = 8)

# Create inset plot
inset_plot <- main_plot + labs(title = "QQ-Plot (zoomed in)") + xlim(-10, 15) + ylim(-15, 20) + 
  theme_clean(base_size = 8) + theme(legend.position = "none", axis.title = element_blank())

inset_grob <- ggplotGrob(inset_plot) # Convert to a grid graphical object

# Final result
final_plot <- main_plot + annotation_custom(grob = inset_grob, xmin = -65, xmax = -10, ymin = 100, ymax = 250) +
  annotate("rect", xmin = -10, xmax = 15, ymin = -15, ymax = 20, color = "black", fill = "gray", size = 0.25, linetype = "dashed", alpha = 0.5) +
  annotate("segment", x = -10, xend = -65, y = -15, yend = 100, linetype = "dashed", color = "black", size = 0.25) +
  annotate("segment", x = 15, xend = -10, y = 20, yend = 250, linetype = "dashed", color = "black", size = 0.25)

# png("question08.png", width = 1920, height = 1080, units = "px", res = 300)
print(final_plot) # Display the resulting plot
# dev.off()