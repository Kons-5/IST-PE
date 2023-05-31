library(ggplot2)

# Leitura do arquivo
dados <- read.delim("C:/Users/andre/Documents/PEs/Ex3/GENDER_EMP_19032023152556091.txt")

# Filtrar os dados para o país e ano desejados
f_data <- subset(dados, Country == "France" & Time == 2018 & IND == "EMP3" & (Age.Group== "15-24" | Age.Group == "25-54" | Age.Group == "55-64") & (Sex=="Men" | Sex=="Women"))

# Criar o gráfico de barras
  ggplot(f_data, aes(x = Age.Group, y = Value, fill = Sex)) +
    geom_bar(position="dodge", stat="identity") +
    xlab("Age group") +
    ylab("Unemployment rate (%)") +
    theme_minimal()
