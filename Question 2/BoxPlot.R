library(tidyverse)
library(viridis)

df <- read.csv("TIME_USE_24092022.csv") %>%
  filter(País !='África do Sul' &
           Sexo == 'Total' &
           (Ocupação=='Cuidados pessoais' | Ocupação=='Trabalho remunerado ou estudo'))

# Remove unwanted column
df$Sexo <- NULL

# Plot the data

scaleFUN <- function(x) sprintf("%.0f", x)

ggplot(df, aes(x = Ocupação, y = Tempo, fill=Ocupação)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  facet_wrap(~Ocupação, scale="free") 

