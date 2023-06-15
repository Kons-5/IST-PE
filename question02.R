# Load required libraries - install.packages("pacman")
pacman::p_load(tidyverse) # Includes ggplot2

# Read and filter the data
df <- read_csv("data/TIME_USE_24092022.csv") %>%
  filter(País !='África do Sul' &
           Sexo == 'Homens' &
           (Ocupação=='Lazer' | Ocupação=='Trabalho não remunerado'))

# Remove unwanted column
df <- df %>% select(-Sexo)

# Function to format the y-axis labels
scaleFUN <- function(x) sprintf("%.0f", x)

# Function to create the y-axis breaks
breaksFUN <- function(x) seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), by = 24)

# Plot the data
plot <- ggplot(df, aes(x = Ocupação, y = Tempo)) +
  geom_boxplot(aes(fill=Ocupação), alpha=0.5) + 
  geom_jitter(aes(color=Ocupação), size = 0.75, shape = 19) +
  coord_flip() + theme_linedraw(base_size = 8)

# Add labels and title 
final_plot <- plot + ggtitle("Análise dos Tempos Médios Diários de Lazer e Trabalho Não Remunerado (Homens)") +
  xlab("") + ylab("Tempo (minutos)") +
  theme(axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text.y = element_blank()) +
  scale_y_continuous(breaks = breaksFUN, labels = scaleFUN) +
  scale_fill_manual(values=c("#95c6b5", "#367ba2")) + 
  scale_color_manual(values=c("#19a742", "#0079ee"))  

# Display the resulting plot
# png("img/question02.png", width = 1920, height = 1080, units = "px", res = 300)
print(final_plot)
# dev.off()