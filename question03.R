`# Load required libraries
library(ggplot2, dplyr)

# Read in data
data <- read.delim("GENDER_EMP_19032023152556091.txt")

# Filter for wanted data
f_data <- data %>%
  filter(Country == "France" & 
         Time == 2018 & 
         IND == "EMP3" & 
         Age.Group %in% c("15-24", "25-54", "55-64") &
         Sex %in% c("Men", "Women"))

# Specify the order of the x-axis
f_data$Age.Group <- factor(f_data$Age.Group, levels = c("15-24", "25-54", "55-64"))

# Develop grouped barplot
final_plot <- ggplot(f_data, aes(x = Age.Group, y = Value, fill = Sex)) +
  geom_bar(position="dodge", stat="identity") +
  ggtitle("Comparison of Unemployment Rates by Age Group and Sex in France, 2018") +
  xlab("Age group") + ylab("Unemployment rate (%)") +
  scale_fill_manual(values = c("Men" = "#C2DCC2", "Women" = "#367ba2")) +
  theme_linedraw(base_size = 8)

png("question03.png", width = 1920, height = 1080, units = "px", res = 300)
print(final_plot) # Display the resulting plot
dev.off()`