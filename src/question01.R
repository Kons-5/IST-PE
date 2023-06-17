# Load required libraries - install.packages("pacman")
pacman::p_load(readxl, tidyverse, reshape2)

# Function to standardize a variable
standardize <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# Read data and build dataframe
data <- read_xlsx("data/econ.xlsx") 
data$tempo <- as.Date(data$tempo, format = "%Y-%m-%d")

df <- data %>%
  filter(tempo >= as.Date("1996-01-01")) %>%
  select(tempo, pop, ndesemp) %>%
  mutate(across(c(pop, ndesemp), standardize))  # Apply variable transformation

# Convert to a melted data frame
meltdf <- melt(df, id="tempo")
print(head(meltdf))

# Create plot
plot <- ggplot(data = meltdf, aes(x=tempo, y=value, colour=variable, group=variable)) +
  geom_line(linewidth=0.5, alpha=0.5, na.rm = TRUE) +
  geom_smooth(data = subset(meltdf, variable == "ndesemp"), method = "gam", se = FALSE, linewidth = 0.6) +
  geom_point(size=0.6, alpha=0.6, na.rm = TRUE)

# Apply theming, labels and title 
scaleFUN <- function(x) sprintf("%.2f", x)

final_plot <- plot + 
  ggtitle("Normalized Trends of Total Population and Unemployment from 1996 Onwards") +
  xlab("Years") + ylab("Normalized values") +
  scale_color_manual(values=c("#34adff","#367ba2"), labels=c("Pop", "Ndesemp")) +
  theme(legend.position = "right", legend.title = element_blank()) +
  theme_linedraw(base_size = 8) +
  scale_x_date(limits=c(as.Date("1995-06-01"), as.Date("2016-01-01")), date_breaks="1.5 years", date_labels="%Y") +
  scale_y_continuous(labels=scaleFUN, breaks=seq(min(meltdf$value, na.rm=TRUE), max(meltdf$value, na.rm=TRUE), by=1))

# Display the resulting plot
# png("img/question01.png", width = 1920, height = 1080, units = "px", res = 300)
print(final_plot)
# dev.off()