setwd("C:/Users/aless/Desktop/MASTER DATA SCIENCE/statistica-descrittiva-per-datascience/5 - Misure di Variabilità con R")

data("mtcars")
attach(mtcars)

#voglio vedere (in generale) come vanno i consumi, legandoli a potenza del motore
# e tipo di motore (a v o a pianale) -> quindi boxplot di mpg vs hp e vs

library(ggplot2)

hp_cl = cut(hp,seq(50,350,50),right=F)
vs_cl = cut(vs,2,right=T) #in questo modo dico a cut di dividere vs in 2 fattori
#la funzione si calcola da sola gli estremi degli intervalli


ggplot(data=mtcars)+
    geom_boxplot(aes(x=vs_cl,y=mpg,fill=hp_cl))+
    labs(title="Cars consumption vs Engine shape",
         x="Engine shape",
         y="Consumption (mpg)",
         fill="Motor power (hp)")+ #il fill messo qui modifica il titolo della legenda
    scale_x_discrete(labels=c("Straight","V-shape"))

#Serve grafico aggiuntivo circa la distribuzione di freq -> uso grafico a barre

ggplot(data=mtcars)+
    geom_bar(aes(x=vs_cl,fill=hp_cl),
             col="black",
             position = "dodge")+
    scale_x_discrete(labels=c("Straight","V-shape"))+
    labs(title="Absolute motor shape distribution",
         x="Engine shape",
         fill="Motor power (hp)") #il fill messo qui modifica il titolo della legenda
    

#qualche misura di variabilità

#voglio confrontare la distribuzione di mpg (consumi) e di cyl (num. cilindri)

#via molto veloce per indici di posizione: uso summary
summary(mpg)


#creo funzione custom per ottenere parametri di variabilità
variability.summary = function(x) {
    n = length(x)
    x_mu = sum(x)/n #oppure mean(x)
    x_IQR = IQR(x)
    x_var = sum((x-x_mu)^2)/n #oppure var(x)
    x_sigma = sqrt(x_var) #oppure sd(x)
    x_CV = x_sigma/x_mu * 100
    
    d = data.frame(n = n, mu=x_mu,variance = x_var,sigma = x_sigma, CV = x_CV)
    return(d)
}

mpg_summary = variability.summary(mpg)
#print("Variability summary for mpg")
print(mpg_summary)
#print("-------------------------")

#variabile cyl
summary(cyl)


cyl_summary = variability.summary(cyl)
#print("Variability summary for cyl")
print(cyl_summary)
#print("-------------------------")


#per fare i confronti si usa soprattutto il CV:
#In questo caso le due distribuzioni sono disperse in modo simile
#poichè hanno CV simile (29.52% vs 28.41%).
#Ricorda: il cv dice che la dev std è il CV% della media
#Ad es. per mpg ho che sigma = 29.5% della media. 









