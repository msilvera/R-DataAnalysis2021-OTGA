#http://daviddalpiaz.github.io/appliedstats/categorical-predictors-and-interactions.html

library(FSA)
library(ggplot2)
library(plotly)
#install.packages("emmeans", dependencies = TRUE)
library(car)

#Carga del paquete FSAdata e informacion del mismo
#Parte a y b)
#------------------------------------
library(FSAdata)
library(help="FSAdata")

#elegir una BBDD del paquete y analizar faltantes
View(InchLake2)
summary(InchLake2)

#

#haciendo el summary se ve que no tiene valores vacios
#data_fish <-subset(x=InchLake2, subset = !is.na(weight)) 
#summary(data_fish)
data_fish <- InchLake2
data_fish <- mutate(data_fish, log_w = log(weight), log_tl=log(length))

#Parte c)
#------------------------------------
#activar variables
attach(data_fish)

#var de respuesta: PESO
#var explicativa cuantitativa: LONGITUD
#var dummy: Año de la muestra


#convierto year a factor para usarla como dummy
year2 <- as.factor(year)

#Parte d)
#------------------------------------
#modelo teorico:
head(data_fish) 

# log(weight_i) = log(α) + β*log(length_i) + δ*year(2008) + γ*log(lenght_i)*year(2008) + ε_i
#para 2007: log(weight_i) = log(α) + β*log(length_i)+ ε_i
#para 2008: log(weight_i) = (log(α) + δ) + (β + γ)*log(length_i) + ε_i

#Los coeficientes a estimar son α, β,δ,γ

#El valor de referencia es el primer nivel del factor de la variable year2 = 2007

#Parte e)
#------------------------------------
#modelo de regresión multiple, considerando la interaccion de las variables log_tl e year
#usando first*second (cross)
mlr <-lm(log_w ~ log_tl*year2)

#Call:
#  lm(formula = log_w ~ log_tl * year2)
#Coefficients:
#  (Intercept)      log_tl          year22008         log_tl:year22008  
#-1.74412           3.20789          -0.29786           0.01021

summary(mlr)


#Call:
#  lm(formula = log_w ~ log_tl * year2)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-1.7574 -0.2346 -0.0654  0.2737  5.7792 

#Coefficients:
#  Estimate Std. Error t value                    Pr(>|t|)    
#(Intercept)      -1.74412    0.08594 -20.295     < 2e-16 ***
#  log_tl            3.20789    0.05425  59.128   < 2e-16 ***
#  year22008        -0.29786    0.10633  -2.801   0.00528 ** 
#  log_tl:year22008  0.01021    0.06429   0.159   0.87383    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 0.4852 on 512 degrees of freedom
#Multiple R-squared:  0.9599,	Adjusted R-squared:  0.9596 
#F-statistic:  4084 on 3 and 512 DF,  p-value: < 2.2e-16

#Parte f)
#------------------------------------
#Analisis del Anova:
#Si afectan a la variable de respuesta weight(log_w)) las variables (PR<0.05): largo (log_tl con PR < 2e-16), año (year2200 con PR = 0.00528)
#No afecta  a la variable de respuesta weight(log_w)) la relación entre las variables year y log_tl (PR=0.87 que es mayor a 0.05): 
#Esto significa que la pendiente del modelo lineal es la misma para ambos años (en el modelo teorico ->  γ = 0)
#Segun el valor del Multiple R-squared, el 95.99% de la variabilidad del peso es explicada por este modelo.


#Parte g)
#------------------------------------
#verificar los supuestos del modelo:
#linealidad, homocedasticidad y normalidad de los errores

#linealidad:
Anova(mlr) #ver análisis del anova en el punto anterior
summary(mlr)
#Multiple R-squared:  0.9599,	Adjusted R-squared:  0.9596  -> Un coeficiente R2 ajustado mayor a 0.7 es indicio de linealidad

#homoceasticidad
plot(x=fitted(mlr), y=residuals(mlr)) #gradico nulo, sin tendencia aparente

#normalidad
hist(residuals(mlr),breaks=60,xlim=c(-2,2)) #la distribución es cercana a la normalidad pero no es exactamente normal, tal como se comprueba mas adelante
boxplot(residuals(mlr))

#comparativa de los coeficientes
#compSlopes(mlr) ->  Error: 'compSlopes' has been removed;
library(emmeans)
comp <-emtrends(mlr,pairwise ~ year2,var="log_tl")
summary(comp, infer=TRUE)

#$emtrends
#year2          log_tl.trend     SE       df      lower.CL upper.CL
#2007          3.21             0.0543    512      3.10     3.31
#2008          3.22             0.0345    512      3.15     3.29
#Confidence level used: 0.95 
#$contrasts
#contrast         estimate      SE        df    t.ratio   p.value
#2007 - 2008      -0.0102       0.0643    512   -0.159    0.8738 

#p.value = 0.8738 - > Se confirma que los slopes para 2007 y 2008 NO son estadísticamente diferentes (lineas paralelas)

#Investigación adicional: 

#Para verificar que tan bien se ajusta el modelo y si se cumplen los supuestos
#podemos hacer el análisis de residuales. Por ejemplo, se pueden calcular 4 tipos de residuales distintos:
#"working", "pearson", "standard", "student"
ei=residuals(mlr, type='working')
pi=residuals(mlr, type='pearson')
di=rstandard(mlr)
ri=rstudent(mlr)
#analizando di:
qqnorm(di);qqline(di)
#qqplot()
#En la gráfica se ve que si bien mayormente parece ajustarse a una distribución normal, en los extremos se separa de la recta.
#El supuesto de normalidad en los errores no se cumple (esto tmb se puede ver con el histograma)

#Para chequear que los errores no están correlacionados, una opción es graficar un diagrama de dispersión del residual versus tiempo (el orden en que las observaciones fueron tomadas)
#Para que se cumpla el supuesto de independencia se espera que los puntos se ubiquen como una nube de puntos sin ningún patrón claro.
#Esto lo podemos ver con las siguientes 3 gráficas:
plot(mlr, which=1:3)
#Analizando las graficas, los diagramas de dispersión entre los residuales no parecen presentar ninguna anomalía.


#El paquete car tiene funciones especiales para crear otro tipo de gráficos de residuales y que son útiles para identificar posibles anomalías en el modelo ajustado.
residualPlots(mlr)
#los graficos muestran una desviacion respecto al "grafico nulo"
#y la salida de la tabla indica que tiene sentido agregar al modelo el valor de log_tl al cuadrado
marginalModelPlots(mlr, las=1)
#Se ve que las lineas roja y azul difieren un poco en algunos sectores-> puede valer la pena agregar el valor de log_tl al cuadrado al modelo.

#se testea un nuevo modelo que incluya log_tl cuadrado y que no considere la interaccion de tl con year (ya que vimos que no afecta)
new_mlr <-lm(log_w ~ log_tl*I(log_tl^2) + year2, data=data_fish )
residualPlots(new_mlr) # residuos con "graficos nulos"
marginalModelPlots(new_mlr, las=1) #el nuevo modelo se ajusta mejor (lineas rojas y azules mas cercanas)
#comparo además la normalidad de los residuales
hist(residuals(new_mlr),breaks=60,xlim=c(-2,2)) #la distribución de los residuos de este nuevo modelo es mas cercana a la normalidad
boxplot(residuals(new_mlr)) #está mejor distribuido

#incluyo también algunos links encontrados con detalle sobre distintos métodos/análisis que se pueden utilizar para validar los supuestos:
#https://fhernanb.github.io/libro_regresion/diag1.html
#https://support.minitab.com/es-mx/minitab/18/help-and-how-to/modeling-statistics/regression/supporting-topics/model-assumptions/validate-model-assumptions/
#https://towardsdatascience.com/q-q-plots-explained-5aa8495426c0


#Parte h)
#------------------------------------
#Grafique las rectas de regresión para cada modelo construido con su variable dummy

f <- ggplot(data = data_fish, 
            aes(color=as.factor(year), x=log_tl, y=log_w))+
      geom_point(size=1, alpha=0.25)+
      geom_smooth(aes(group=as.factor(year)), method = "lm")+
      labs(x="log (Total Length)", y= "log(weight)")+
      theme_classic()


ggplotly(f)


#Graficas adicionales para comparar visualmente los 2 modelos creados:

plot(mlr$fitted.values ,data_fish$log_w,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)

plot(new_mlr$fitted.values ,data_fish$log_w,
     xlab="predicted",ylab="actual")
abline(a=0,b=1)

