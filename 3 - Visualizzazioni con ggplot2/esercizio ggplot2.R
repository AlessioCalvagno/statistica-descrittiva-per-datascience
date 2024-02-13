setwd("C:/Users/aless/Desktop/MASTER DATA SCIENCE/statistica-descrittiva-per-datascience/3 - Visualizzazioni con ggplot2")

install.packages("ggplot2")
library(ggplot2)

data("iris")

attach(iris)

#faccio grafico di distribuzine assoluta di Species
ggplot((data=iris))+
    geom_bar(aes(x=Species),
             stat = "count",
             col="black",
             fill="blue")+
    labs(title="Absolute Species frequency distribution")+
    theme_bw()
#labs si occupa di titolo e etichette assi

#faccio grafico di distribuzioni assolute di Petal.lenght diviso in classi,
#facendo confronto con variabile Species

lunghezza_cl = cut(Petal.Length,seq(1,7,2),right = F)

ggplot(data=iris)+
    geom_bar(aes(x=lunghezza_cl,fill=Species),
             col="black",
             position = "dodge")


#provo a fare box plot
ggplot(data=iris)+
    geom_boxplot(aes(x=Species,y=Petal.Length),fill="lightblue")+
    labs(title="Boxplot of Petal length vs Species")+
    theme_gray()
