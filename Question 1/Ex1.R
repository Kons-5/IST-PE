library(readxl)
library(ggplot2)

data <- read_xlsx("C:/Users/andre/Documents/PEs/Ex1/econ.xlsx") 
data$tempo <- as.Date(data$tempo, format = "%Y-%m-%d")
f_data <- subset(data, tempo >= as.Date("1996-01-01"))
f_data <- f_data[,c("tempo", "pop", "ndesemp")]

#Média e desvio padrão da variável "pop"
xk_pop <- mean(f_data$pop)
sd_pop <- sd(f_data$pop)

#Média e desvio padrão da variável "ndesemp"
xk_ndesemp <- mean(f_data$ndesemp)
sd_ndesemp <- sd(f_data$ndesemp)

f_data[,c("pop")] <- (f_data[,c("pop")] - xk_pop) / sd_pop
f_data[,c("ndesemp")] <- (f_data[,c("ndesemp")] - xk_ndesemp) / sd_ndesemp

ggplot(f_data, aes(x = tempo)) + 
  geom_line(aes(y = pop, color = "Pop")) +
  geom_line(aes(y = ndesemp, color = "Ndesemp")) +
  labs(x = "Anos", y = "", title = "") +
  scale_color_manual(values = c("Pop" = "blue", "Ndesemp" = "red")) +
  theme_minimal()+
  scale_x_date(date_breaks = "1 years",
               date_labels = "%y")+
  scale_y_continuous(breaks = seq(-2.25, 2.25, 0.25), 
                  limits=c(-2.25, 2.25))


