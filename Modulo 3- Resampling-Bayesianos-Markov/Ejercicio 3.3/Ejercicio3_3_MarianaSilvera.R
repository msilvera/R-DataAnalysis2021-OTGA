library(FSAdata)
library(MASS)
library(dplyr)


#library(help="FSAdata")
#cargo los datos
#data <- WalleyeErie2
summary(WalleyeErie2)
data <-subset(x=WalleyeErie2, subset = !is.na(w)) #elimino los datos incompletos
summary(data)

set.seed(1) # semilla para el random
data <- data %>% mutate_at(vars("age"), factor) # transformo en factor la comlumna edad

#extraigo el 80% de los datos para entrenamiento
intrain <- sample(1:nrow(data), size = round(0.8*nrow(data)))

#genero modelo para la edad en base a las demas variables
lda.fit <-lda(age~. , data= data, subset= intrain)
lda.fit

#verifico que tan bien se comporta el discriminante lineal generado
lda.pred <- predict(lda.fit, data)
names(lda.pred)

#obtenfo la clase
lda.class <- lda.pred$class
#construyo la matriz
table(lda.class, data$age)
#veo que tan bien se ajusta, utilizando la media
mean(lda.class==data$age) 

#resultado, desempeño de : 0.6571231