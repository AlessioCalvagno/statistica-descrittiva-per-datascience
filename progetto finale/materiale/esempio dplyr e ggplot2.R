library(dplyr)
library(ggplot2)

# Creiamo un data frame di esempio
df <- data.frame(
    gruppo = rep(c("A", "B"), each = 3),
    variabile = c(10, 12, 8, 6, 5, 7)
)

# Raggruppiamo il data frame per la colonna "gruppo" e calcoliamo la media per gruppo
df_grouped <- df %>% group_by(gruppo) %>% summarize(media = mean(variabile))

# Creiamo un grafico a barre delle medie condizionate
ggplot(df_grouped, aes(x = gruppo, y = media)) +
    geom_bar(stat = "identity") +
    labs(x = "Gruppo", y = "Media") +
    ggtitle("Medie condizionate per gruppo")


#Generato da ProfAI - https://prof.profession.ai/