library(dplyr)
library(ggplot2)

#filename <- file.choose()
filename <-"C:\\Users\\msilvera\\Documents\\ONG\\OceanTeacher\\Modulo 3\\Ejercicio 3.2\\precipitacion_caribe.csv"
df <- read.csv(filename)

#Me quedo con los datos de precipitaciones del depto:
data_precip <- df %>% filter(Departamento == "SUCRE")

summary(data_precip)
#chequeo el sesgo de los valores hacia el cero
hist(data_precip$ValorObservado)

#convertir el campo fecha a Date
data_precip$FechaObservacion <- as.POSIXct(data_precip$FechaObservacion)

#Grafico comportamiento a traves del tiempo
ggplot(data_precip,aes(x=FechaObservacion, y=ValorObservado)) + geom_line()

#Restructuro los datos para tener dos variables aleatorias y una variable de respuesta
#de la precipitacion de los 2 dias anteriores, calculo la del 3er día
ts.data_precip <- data.frame(x1 = data_precip[1:(nrow(data_precip)-2), "ValorObservado"],
                              x2 = data_precip[2:(nrow(data_precip)-1), "ValorObservado"],
                              y = data_precip[3:nrow(data_precip), "ValorObservado"])

#Approach via "Conjuntos de Validacion"
#----------------------------------
set.seed(1) #para el random
#me quedo con un subset aleatorio del dataset original
ds.train <- sample(nrow(ts.data_precip), floor(0.8*nrow(ts.data_precip)))

lm.fit <- lm(y~x1+x2, data=ts.data_precip, subset = ds.train)
coef(lm.fit)
#verifico errores cuadrados medios
mean((ts.data_precip$y - predict(lm.fit, ts.data_precip))[-ds.train]^2)

#Cambio el seed al random para validar que los resultados varian

set.seed(2) #cambio el random
#me quedo con un subset aleatorio del dataset original
ds.train <- sample(nrow(ts.data_precip), floor(0.8*nrow(ts.data_precip)))

lm.fit <- lm(y~x1+x2, data=ts.data_precip, subset = ds.train)
coef(lm.fit)
#verifico errores cuadrados medios
mean((ts.data_precip$y - predict(lm.fit, ts.data_precip))[-ds.train]^2)


#Approach via "Leave one Out Cross Validation"
#----------------------------------
#Se utiliza glm para poder usar luego cv.glm

glm.fit <- glm(y~x1+x2, data=ts.data_precip)
coef(glm.fit)

library(boot)
cv.err <- cv.glm(ts.data_precip, glm.fit) #este demora en ejecutar
cv.err$delta #resultado


#Approach via "K-Fold"
#----------------------------------

set.seed(17)
cv.err <- cv.glm(ts.data_precip, glm.fit, K=10) #especifico que quiero 10 folds. Ya no demora en ejecutar.
cv.err$delta #resultado


#Approach via "Bootstrap"
#----------------------------------
#Estimar precision de un modelo de regresion lineal

#defino una funcion  que devuelve los coeficientes del modelo de regresion lineal:
boot.fn <- function(data, index) 
                return( coef(lm(y~x1+x2, data=data, subset = index)))
boot.fn(ts.data_precip, 1:nrow(ts.data_precip))

set.seed(1)

#ejecuto solo una vez, con reemplazo
boot.fn(ts.data_precip, sample(nrow(ts.data_precip),nrow(ts.data_precip), replace = T))

#ejecuto 1000 iteraciones y se imprimen los resultados
boot(ts.data_precip, boot.fn, 1000)


#Estimar precision estadistico de interés
#var(x)=1, var(y)=1.25 y covarianza XY = 0.5, siendo X , Y dos vars aleatorias

#genero 100 datos que cumplan con las caracteristicas definidas para X Y a partir de una dist normal
set.seed(10)
m <- 100
Z1 <- rnorm(m, 0, sqrt(0.5))
Z2 <- rnorm(m, 0, sqrt(0.75))
W <- rnorm(m, 0, sqrt(0.5))

data <- data.frame(X = Z1 + W,
                   Y = Z2 + W)
#verifico: (cuanto mayor el m usado mas cercanos los valores)
var(data$X)
var(data$Y)
cov(data$X,data$Y)

#funcion para calculo de alfa

alpha.fn <- function(data, index) 
            {
                X <- data$X[index]
                Y <- data$Y[index]
                
                return( 
                        (var(Y)-var(X))/(var(X)-var(Y) + 2* cov(X,Y)) 
                      )
}

alpha.fn(data, sample(100,100,replace = T))      

boot(data, alpha.fn,R=1000)
