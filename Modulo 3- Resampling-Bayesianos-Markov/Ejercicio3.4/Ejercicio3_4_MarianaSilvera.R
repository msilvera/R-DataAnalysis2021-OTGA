library(dplyr)
library(ggplot2)
library(markovchain)

#filename <- file.choose()
filename <-"C:\\Users\\msilvera\\Documents\\ONG\\OceanTeacher\\Modulo 3\\Ejercicio 3.2\\precipitacion_caribe.csv"
df <- read.csv(filename)

#Me quedo con los datos de precipitaciones del depto:
data_precip <- df %>% filter(Departamento == "SUCRE")

summary(data_precip)
hist(data_precip$ValorObservado)

data_precip$FechaObservacion <- as.POSIXct(data_precip$FechaObservacion)
#verifico los datos graficamente:
ggplot(data_precip, aes(x=FechaObservacion, y=ValorObservado)) + geom_line()

#Defino las etiquetas para los estados
estados <- c("not rain", "low", "normal","heavy rain","very heavy rain")

#Genero una nueva columna agrupando por estado
data_precip$Estado <- cut( data_precip$ValorObservado,
                           breaks = c(0,0.02,0.25,0.5,1,Inf),
                           labels = estados,
                           include.lowest = T
                           )       

#genero una nueva tabla con los datos en el formato necesario para aplicar markov: DATO ACTUAL -> DATO SIGUIENTE
data.markov <- data.frame(state0 = data_precip[1:nrow(data_precip)-1, "Estado"],
                          state1 = data_precip[2:nrow(data_precip), "Estado"]
                          )
#matriz de Markov, cant de transiciones entre estados
table(data.markov)
markov.mat <- as.data.frame.matrix(table(data.markov))

#Matriz de probabilidades de transicion entre estados
P <- t(markov.mat)/rowSums(markov.mat)

#cadena de markov
mc <- new("markovchain", transitionMatrix=P, states=estados, name="CM Precipitacion")
summary(mc)
#grafico la cadena
plot(mc)

#simulo "n" días partiendo del estado inicial X0:
n <- 5
X0 <- c(0.2,0.2,0.2,0.2,0.2)
Xn <- X0*(mc^n)

plot(mc^n)

#Distribucion de los estados ya con convergencia/estabilizados:
DistEst <- steadyStates(mc)
DistEst

#tiempo necesario para que se estabilicen:
M <- 1/DistEst
M
