library(tidyverse)
library(ggthemr)

df <- read.csv("TIME_USE_24092022.csv") %>%
  filter(País !='África do Sul' &
           Sexo == 'Total' &
           (Ocupação=='Cuidados pessoais' | Ocupação=='Trabalho remunerado ou estudo'))

# Remove unwanted column
df$Sexo <- NULL

# Function to format the y-axis labels
scaleFUN <- function(x) sprintf("%.0f", x)

# Function to create the y-axis breaks
breaksFUN <- function(x) seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), by = 45.1)

# Set plot theme
#ggthemr("light")

# Plot the data
ggplot(df, aes(x = Ocupação, y = Tempo, fill=Ocupação)) +
  geom_boxplot(alpha=0.4) +
  geom_jitter(data = df, aes(x = Ocupação, y = Tempo, color=Ocupação), size=1.45) +
  facet_wrap(~Ocupação, scale="free") + xlab("") +
  theme(axis.text.x = element_blank(), 
        axis.ticks = element_blank())
