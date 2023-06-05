library(tidyverse)
library(ggthemr)

df <- read.csv("TIME_USE_24092022.csv") %>%
  filter(País !='África do Sul' &
         Sexo == 'Homens' &
        (Ocupação=='Lazer' | Ocupação=='Trabalho não remunerado'))

# Remove unwanted column
df$Sexo <- NULL

# Function to format the y-axis labels
scaleFUN <- function(x) sprintf("%.0f", x)

# Function to create the y-axis breaks
breaksFUN <- function(x) seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), by = 50)

# Set plot theme
ggthemr("flat")

# Plot the data
plot <- ggplot(df, aes(x = Ocupação, y = Tempo, fill=Ocupação)) +
        geom_boxplot(alpha=0.4) + 
        geom_jitter(size = 1.5, shape = 24) +
        coord_flip() 

# Add labels and title (coord_flips rotates the axis)
plot + ggtitle("Análise dos Tempos Médios Diários de Lazer e Trabalho Não Remunerado (Homens)") + ylab("Tempo") + xlab("") +
       theme(axis.text.y = element_blank(), 
              axis.ticks = element_blank(),
              strip.background = element_blank(),
              strip.text.y = element_blank(),
              plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(breaks = breaksFUN, labels = scaleFUN)


