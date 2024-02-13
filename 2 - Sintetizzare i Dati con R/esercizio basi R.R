#imposto la working directory di dove si trova il file .R
setwd("C:/Users/aless/Desktop/MASTER DATA SCIENCE/statistica-descrittiva-per-datascience/2 - Sintetizzare i Dati con R")

dati = read.csv("tonni.csv",sep=";")
#dati è un Dataframe

#Dimensionalità campione = numero righe del dataframe
N = dim(dati)[1]
print(N)

## FACCIO ESERCIZIO DI DEJAN PER LA VAR. PESO

#Nota importante: quando si hanno poche osservazioni, per le variabili numeriche,
#conviene dividere gli elementi in classi. Ogni classe è un intervallo di valori della
#variabile. Ogni elemento deve rientrare in una sola classe.

#divido in classi con la funzione cut
#vedo quali sono il max e il min della variabile per avere un'idea su come 
#suddividere in classi

min(dati["PESO"]) #25
max(dati["PESO"]) #299
#creo una nuova colonna per salvare la suddivisione in classi
dati["PESO_CL"] = cut(dati$PESO,breaks=c(25,100,200,300), right = F)


#la distribuzione di frequenza assoluta si ottiene con la funzione table
ni = table(dati["PESO_CL"])

#la distr. di freq. relativa si ottiene dividendo i singoli conteggi per il totale di
#elementi
fi = ni/N 

#ottengo la distribuzione cumulata assoluta con cumsum
Ni = cumsum(ni)

#Distribuzione cumulata relativa = cumulata/N
Fi = Ni/N

distribuzioni_peso = cbind(ni,fi,Ni,Fi) #cbind ritorna matrix
print(distribuzioni_peso)

#passo al dataframe
distribuzioni_peso_DF = as.data.frame(distribuzioni_peso)

write.csv(distribuzioni_peso_DF,"distribuzioni peso.csv")

########################################
##Grafici

#provo grafico a torta e a barre di distribuzione assoluta
pie(distribuzioni_peso_DF$ni,
    labels = rownames(distribuzioni_peso_DF),
    main = "Absolute weight distribution",
    col = c("green1","green3","green4"),
    radius = 1,
    )

barplot(distribuzioni_peso_DF$ni,
        xlab = "Weigth classes (g)",
        ylab = "Absolute frequency",
        main="Absolute weight distribution",
        names.arg = rownames(distribuzioni_peso_DF))



