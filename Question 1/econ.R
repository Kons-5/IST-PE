library(readxl)
library(tidyverse)
library(reshape2)

df <- read_excel("~/Documents/IST-PE/Question\ 1/econ.xlsx")[223:574, c(1, 4:5)]

# Apply variable transformation
stdd_tpp = sd(df[[2]], na.rm = TRUE)
mean_tpp = mean(df[[2]], na.rm = TRUE)

stdd_ddesemp = sd(df[[3]], na.rm = TRUE)
mean_ddesemp = mean(df[[3]], na.rm = TRUE)

for(i in 1:nrow(df)) {    
  df[i, 2] <- (df[i, 2] - mean_tpp)/stdd_tpp
  df[i, 3] <- (df[i, 3] - mean_ddesemp)/stdd_ddesemp
}

# Plot
# Convert to a melted data frame
meltdf <- melt(df, id="tempo")

# Create plot
plot <- ggplot(data = meltdf, aes(x=tempo, y=value, colour=variable, group=variable)) +
        geom_line(linewidth=0.5, alpha=0.5,na.rm = TRUE) +
        geom_point(size=0.7, alpha=0.6,na.rm = TRUE) +
        geom_smooth(na.rm = TRUE, alpha=0.2) +
        scale_color_manual(values=c("#D65DB1", "#9270D3"), labels=c("TPP", "DDesemp")) 

# Add theming
lims <- as.POSIXct(strptime(c("1986-01-01","2015-04-01"), format = "%Y-%m-%d"))   
scaleFUN <- function(x) sprintf("%.2f", x)

plot + 
    theme_minimal() +
    theme(legend.position = "right",
          legend.title = element_blank(),
          panel.grid.major = element_line(colour = "grey90"),
          panel.grid.minor = element_line(colour = "grey90")) +
    labs(y = "", x = "Anos") +
    scale_x_datetime(date_labels = "%Y",
                     date_breaks = "25 month",
                     limits = lims,
                     expand = expansion(add = c(0,0))) +
   scale_y_continuous(labels=scaleFUN, 
                      breaks = seq(min(meltdf$value, na.rm = TRUE), 
                                   max(meltdf$value, na.rm = TRUE), by = 1))









