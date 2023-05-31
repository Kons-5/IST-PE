library(readxl)
library(ggplot2)


dados <- read.csv("C:/Users/andre/Documents/PEs/Ex2/TIME_USE_24092022.csv", header = TRUE, sep = ",")

# Retirar dados da África do Sul e restringir dos dados a Lazer a Trabalho não remunerado
dados <- dados[dados$País != "África do Sul",]
dados_filtrados <- subset(dados, Ocupação %in% c("Lazer", "Trabalho não remunerado") & Sexo == "Homens")

ggplot(dados_filtrados, aes(x = Ocupação, y = Tempo, fill = Ocupação)) +
  geom_boxplot(alpha=0.4) +
  scale_fill_manual(values = c("Lazer" = "blue", "Trabalho não remunerado" = "red")) +
  labs(x = "Ocupação", y = "Tempo médio diário (minutos)", fill = "Ocupação")+
  scale_y_continuous(breaks = seq(20, 410, 40), 
                     limits=c(20, 405))+
  geom_jitter(data = dados_filtrados, aes(x = Ocupação, y = Tempo, color=Ocupação)) +
  theme_minimal()