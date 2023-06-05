# Set designated seed 
set.seed(1338)

# Cauchy's distribuition parameters
location = 2.2
scale = 1.6

# Normal's distribuition parameters
mean = 3.9
sd = sqrt(1.4)

# Generate sample
sample_size = 124
sample = rcauchy(sample_size, location, scale)

# Find theoretical quantiles
quantiles = (1:sample_size) / (sample_size + 1)
theoretical_quantiles_cauchy = qcauchy(quantiles, location, scale)
theoretical_quantiles_norm = qnorm(quantiles, mean, sd)

# sort sample
sample = sort(sample)

# create plot
main_plot <- ggplot() +
          geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
          geom_point(aes(x = theoretical_quantiles_cauchy, y = sample, colour="Cauchy"), shape=1,alpha=0.5, size=2) +
          geom_point(aes(x = theoretical_quantiles_norm, y = sample, colour="Normal"), shape=1,alpha=0.5,size=2) +
          labs(title = "QQ-Plot", x = "Theoretical Quantiles", y = "Sample Values", color = "Distribuitons") +
          scale_color_manual(values = c("Cauchy" = "blue", "Normal" = "red")) +
          theme_linedraw() 

# Create inset plot
inset_plot <- main_plot + xlim(-6,14) + ylim(-20,20) +
              theme(legend.position = "none", axis.title = element_blank(), title = element_blank())

inset_grob <- ggplotGrob(inset_plot)
final_plot <- main_plot + annotation_custom(grob = inset_grob, xmin = -65, xmax = -10, ymin = 100, ymax = 250)
print(final_plot)