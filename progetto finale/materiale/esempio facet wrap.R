rm(list= ls())
library(ggplot2)
data("iris")
# esempio di uso di facet_wrap() su dataset iris
ggplot(data = iris, aes(x = Sepal.Width, y = Sepal.Length)) +
    geom_point() +
    facet_wrap(~Species)+
    theme_hc()


#Generato da ProfAI - https://prof.profession.ai/


ggplot(data = iris, aes(x = Sepal.Width, y = Sepal.Length)) +
geom_point() +
    facet_wrap(~Species, nrow = 3, ncol = 2, 
               labeller = labeller(Species = c(setosa = "Setosa", 
                                               versicolor = "Versicolor", 
                                               virginica = "Virginica")),
               strip.position = "top")+
    theme_hc()+
    theme(strip.background = element_blank())


# Generato da ProfAI - https://prof.profession.ai/


# Esempio di uso di facet_wrap() personalizzando le etichette con il dataset mtcars
data("mtcars")
ggplot(data = mtcars, aes(x = mpg, y = hp)) +
    geom_point() +
    # facet_wrap(~ cyl + gear, nrow = 2, labeller = labeller(cyl = c("4 Cilindri","6 Cilindri", "8 Cilindri"),
    #                                                        gear = c("3 Ingranaggi","4 Ingranaggi","5 Ingranaggi")))
    facet_wrap(~ cyl + gear, nrow = 2, labeller = "label_both")
