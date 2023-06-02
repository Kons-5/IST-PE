library(readxl)
library(tidyverse)
library(reshape2)

# build dataframe
data <- read_xlsx("econ.xlsx") 

data$tempo <- as.Date(data$tempo, format = "%Y-%m-%d")
df <- subset(data, tempo >= as.Date("1996-01-01"))
df <- df[,c("tempo", "pop", "ndesemp")]

# Apply variable transformation
stdd_pop = sd(df[[2]], na.rm = TRUE)
mean_pop = mean(df[[2]], na.rm = TRUE)

stdd_ndesemp = sd(df[[3]], na.rm = TRUE)
mean_ndesemp = mean(df[[3]], na.rm = TRUE)

df[,c("pop")] <- (df[,c("pop")] - mean_pop) / stdd_pop
df[,c("ndesemp")] <- (df[,c("ndesemp")] - mean_ndesemp) / stdd_ndesemp

# Plot
# Convert to a melted data frame
meltdf <- melt(df, id="tempo")

# Create plot
plot <- ggplot(data = meltdf, aes(x=tempo, y=value, colour=variable, group=variable)) +
  geom_line(linewidth=0.5, alpha=0.5,na.rm = TRUE) +
  geom_smooth(data = subset(meltdf, variable == "ndesemp"), method = "gam") +
  geom_point(size=0.7, alpha=0.6,na.rm = TRUE) +
  scale_color_manual(values=c("#D65DB1", "#9270D3"), labels=c("POP", "NDesemp")) 

# Add theming
scaleFUN <- function(x) sprintf("%.2f", x)

plot + theme_minimal() +
       theme(legend.position = "right",
             legend.title = element_blank(),
             panel.grid.major = element_line(colour = "grey90"),
             panel.grid.minor = element_line(colour = "grey90")) +
             labs(y = "", x = "Anos") +
             scale_x_date(date_breaks = "1.5 years",
                          date_labels = "%Y")
             scale_y_continuous(labels=scaleFUN,
                                breaks = seq(min(meltdf$value, na.rm = TRUE),
                                max(meltdf$value, na.rm = TRUE), by = 1))










