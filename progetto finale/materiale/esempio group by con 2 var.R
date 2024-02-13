library(dplyr)

# Creiamo un data frame di esempio
df <- data.frame(
    gruppo1 = rep(c("A", "B"), each = 3),
    gruppo2 = rep(c("X", "Y", "Z"), 2),
    variabile = c(1, 2, 3, 4, 5, 6)
)

# Raggruppiamo il data frame per le colonne "gruppo1" e "gruppo2"
df_grouped <- df %>% group_by(gruppo1, gruppo2)

# Esegui altre operazioni sui gruppi, ad esempio calcola la somma della variabile per ogni combinazione di gruppo1 e gruppo2
summary <- df_grouped %>% summarize(somma_variabile = sum(variabile),n=n())

# Stampiamo il risultato
print(summary)


#Generato da ProfAI - https://prof.profession.ai/