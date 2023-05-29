library(tidyverse)
library(ggthemr)

# read relevant rows from .txt
df <- read.delim( 'GENDER_EMP_19032023152556091.txt') %>%
  filter(Country =='Slovak Republic' & 
           IND == 'EMP1' & 
           SEX != 'ALL_PERSONS' & 
           TIME == '2010' & 
           (AGE != '1564' & AGE != 'TOTAL'))

# Remove unwanted collumns
df <- df[,-c(1:5,9:16, 18:19)]

# Set plot theme
ggthemr("fresh")

# Develop grouped barplot
ggplot(df, aes(fill=Sex, y=Value, x=Age.Group)) + 
  geom_bar(position="dodge", stat="identity") +
  ylab("Labour force participation rate") +
  xlab("Age group")