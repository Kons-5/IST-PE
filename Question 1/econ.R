library(readxl)
library(tidyverse)
library(extrafont)

df <- read_excel("econ.xlsx")[223:574, c(1, 4:5)]

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
lims <- as.POSIXct(strptime(c("1986-01-01","2015-04-01"), format = "%Y-%m-%d"))    

meltdf <- melt(df,id="tempo")
plot <- ggplot(meltdf,aes(x=tempo,y=value,colour=variable,group=variable)) +
        geom_line(linewidth=0.5, alpha=0.6) + geom_point(size=0.7) +
        scale_color_manual(values=c("#D65DB1", "#9270D3"),labels=c("TPP", "DDesemp")) +
        theme_bw()

# Plot Theming
plot + theme(legend.position = "right") +
       theme(legend.title = element_blank()) +
       theme(legend.key = element_rect(fill = "white", colour = "black")) +
       theme(text=element_text(family="CM Sans", face="bold", size=12)) +
       labs(y = "", x = "Anos") +
       scale_x_datetime(date_labels = "%Y",
                         date_breaks = "25 month",
                         limits = lims,
                         expand = expansion(add = c(0,0))) 





