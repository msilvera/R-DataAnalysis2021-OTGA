packages <- c("data.table", "naniar", "mice", "dplyr", "Rmisc", "readxl", "ggplot2", "tidyverse")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(packages)

#Agrego options para que no transforme a notacion científica los datos del csv
options(scipen = 999)

#EJERCICIO:  ALGORITMOS DE DETECCION DE DATOS ATIPICOS

filename = file.choose()
#[1] "C:\\Users\\msilvera\\Documents\\ONG\\OceanTeacher\\Modulo 2\\Ejercicio Imputacion\\datos_ejercicio-AtipicosImputacion.csv
#uso read.csv porque los datos están separados por comas
datos <- read.csv(filename, stringsAsFactors = FALSE)

#Me quedo con las columnas que me interesan, incluyendo el rowID para identificar las filas 
datosf <- subset(datos, select = c("ID_MUESTREO","COD_ESTACION","REGION_DES","SUS_ESTACION_DES",
                                   "CODIGO_VARIABLE_EV_DES","CONCENTRACION_EV"))


#traspongo los datos con tidyverse, de rows a columnas
#hay algunos datos duplicados -> para poder trasponder agrego: values_fn = mean
df <- datosf %>% pivot_wider(names_from = CODIGO_VARIABLE_EV_DES, values_from = CONCENTRACION_EV,values_fn = mean)

test <- df %>% add_any_miss()
test <- df %>% add_any_miss(Salinidad, Nitritos)


#renombro columnas porque el espacio no le gusta a la funcion de mice
#Lo correcto es hacer una funcion que recorra todas las columnas y las normalice

names(df)[names(df)=="Nut. Ortofosfatos"] <- "Nut_Ortofosfatos"
names(df)[names(df)=="Porcentaje de Saturación de Oxígeno"] <- "Porcentaje_Saturacion_Oxigeno"
names(df)[names(df)=="Oxigeno Disuelto"] <- "Oxigeno_Disuelto"
names(df)[names(df)=="Sólidos Suspendidos Totales"] <- "Solidos_Suspendidos_Totales"
names(df)[names(df)=="Nut. Amonio"] <- "Nut_Amonio"
names(df)[names(df)=="Coliformes totales"] <- "Coliformes_totales"
names(df)[names(df)=="Coliformes termotolerantes"] <- "Coliformes_termotolerantes"
names(df)[names(df)=="Solidos Suspendidos Totales"] <- "Solidos_Suspendidos_Totales"

#Elimino los coliformes que no me interesan
df <- df %>% select(-Coliformes_totales)
df <- df %>% select(-Coliformes_termotolerantes)
df <- df %>% select(-Solidos_Suspendidos_Totales)

# NUMERO DE FALTANTES POR VARIABLE

gg_miss_var(df, show_pct = TRUE) + ylim(0, 100)
gg_miss_var(df, REGION_DES, show_pct = TRUE) + ylim(0, 100)

#Porcentaje de faltantes x fila
test <- df
test$prcNa <- prop_miss_row(df)



# DISTRIBUCION NA

density_na <- multiplot(ggplot(data = df[!complete.cases(df),3:13], aes(Conductividad, color = REGION_DES)) + geom_density() + theme_classic(),
                        ggplot(data = df[!complete.cases(df),3:13], aes(Salinidad, color = REGION_DES)) + geom_density() + theme_classic(),
                        ggplot(data = df[!complete.cases(df),3:13], aes(Nitritos, color = REGION_DES)) + geom_density() + theme_classic(),
                        ggplot(data = df[!complete.cases(df),3:13], aes(Nitratos, color = REGION_DES)) + geom_density() + theme_classic(),
                        ggplot(data = df[!complete.cases(df),3:13], aes(Temperatura, color = REGION_DES)) + geom_density() + theme_classic(),
                        ggplot(data = df[!complete.cases(df),3:13], aes(`Oxigeno_Disuelto`, color = REGION_DES)) + geom_density() + theme_classic(),
                        ggplot(data = df[!complete.cases(df),3:13], aes(`Porcentaje_Saturacion_Oxigeno`, color = REGION_DES)) + geom_density() + theme_classic(),
                                                cols = 3)

density_na <- multiplot(ggplot(data = df[!complete.cases(df),3:13], aes(Conductividad, color = SUS_ESTACION_DES)) + geom_density() + theme_classic(),
                        ggplot(data = df[!complete.cases(df),3:13], aes(Salinidad, color = SUS_ESTACION_DES)) + geom_density() + theme_classic(),
                        ggplot(data = df[!complete.cases(df),3:13], aes(Nitritos, color = SUS_ESTACION_DES)) + geom_density() + theme_classic(),
                        ggplot(data = df[!complete.cases(df),3:13], aes(Nitratos, color = SUS_ESTACION_DES)) + geom_density() + theme_classic(),
                        ggplot(data = df[!complete.cases(df),3:13], aes(Temperatura, color = SUS_ESTACION_DES)) + geom_density() + theme_classic(),
                        ggplot(data = df[!complete.cases(df),3:13], aes(`Oxigeno_Disuelto`, color = SUS_ESTACION_DES)) + geom_density() + theme_classic(),
                        ggplot(data = df[!complete.cases(df),3:13], aes(`Porcentaje_Saturacion_Oxigeno`, color = SUS_ESTACION_DES)) + geom_density() + theme_classic(),
                        cols = 3)
#Me quedo con la región Caribe y agua marina para el ejercicio
datosEj <- df[df$REGION_DES == "Caribe" & df$SUS_ESTACION_DES == "Agua Marina",]

#IMPUTACION MICE


get_data_imputed <- function(x, methodx){
  
  imputed_data <- mice(x[,5:ncol(x)], m = 5, maxit = 5, method = methodx)
  d_plot <- densityplot(imputed_data)
  data_complete <- mice::complete(imputed_data)
  
  resultado <- list(imputed_data = imputed_data, plot = d_plot, complete_data = data_complete)
  
  return(resultado)
}

#Se comparan los siguientes 3 metodos de la funcion MICE:

resultado_sample <- get_data_imputed(datosEj, "sample")
resultado_cart <- get_data_imputed(datosEj, "cart")
resultado_norm <- get_data_imputed(datosEj, "norm.nob")

resultado_sample
resultado_cart
resultado_norm

fill_data_miss <- function(x, imputed_result){
  
  data <- x
  
  for(i in 1:nrow(data)){
    
    for(j in 5:ncol(data)){
      
      if(is.na(data[i,j])){
        
        data[i,j] <- imputed_result[i, j-4]
        
      }
    }
  }
  
  return(data)
}


datos_sample <- fill_data_miss(setDF(datosEj), resultado_sample$complete_data)
datos_cart <- fill_data_miss(setDF(datosEj), resultado_cart$complete_data)
datos_norm <- fill_data_miss(setDF(datosEj), resultado_norm$complete_data)

longer_sample <- melt(setDT(datos_sample), id.vars = c("ID_MUESTREO","COD_ESTACION","REGION_DES","SUS_ESTACION_DES"),
                   measure.vars = list(value = c("Nut_Ortofosfatos","Porcentaje_Saturacion_Oxigeno","Conductividad","Salinidad",
                                                 "Nitritos","Nitratos","Oxigeno_Disuelto","Temperatura","Nut_Amonio")))[, variable := factor(variable, labels = c("Nut_Ortofosfatos","Porcentaje_Saturacion_Oxigeno","Conductividad","Salinidad",
                                                                                                                                                                  "Nitritos","Nitratos","Oxigeno_Disuelto","Temperatura","Nut_Amonio"))]
longer_cart <- melt(setDT(datos_cart), id.vars = c("ID_MUESTREO","COD_ESTACION","REGION_DES","SUS_ESTACION_DES"),
                      measure.vars = list(value = c("Nut_Ortofosfatos","Porcentaje_Saturacion_Oxigeno","Conductividad","Salinidad",
                                                    "Nitritos","Nitratos","Oxigeno_Disuelto","Temperatura","Nut_Amonio")))[, variable := factor(variable, labels = c("Nut_Ortofosfatos","Porcentaje_Saturacion_Oxigeno","Conductividad","Salinidad",
                                                                                                                                                                     "Nitritos","Nitratos","Oxigeno_Disuelto","Temperatura","Nut_Amonio"))]
longer_norm <- melt(setDT(datos_norm), id.vars = c("ID_MUESTREO","COD_ESTACION","REGION_DES","SUS_ESTACION_DES"),
                      measure.vars = list(value = c("Nut_Ortofosfatos","Porcentaje_Saturacion_Oxigeno","Conductividad","Salinidad","Nitritos","Nitratos","Oxigeno_Disuelto","Temperatura","Nut_Amonio")))[, variable := factor(variable, labels = c("Nut_Ortofosfatos","Porcentaje_Saturacion_Oxigeno","Conductividad","Salinidad","Nitritos","Nitratos","Oxigeno_Disuelto","Temperatura","Nut_Amonio"))]

#adiciono columna informativa
longer_sample <- longer_sample %>% mutate(imp = "sample")
longer_cart <- longer_cart %>% mutate(imp = "cart")
longer_norm <- longer_cart %>% mutate(imp = "norm")

#Manipulo el dataset original
datos_wide <- melt(setDT(datosEj), id.vars = c("ID_MUESTREO","COD_ESTACION","REGION_DES","SUS_ESTACION_DES"),
                    measure.vars = list(value = c("Nut_Ortofosfatos","Porcentaje_Saturacion_Oxigeno","Conductividad","Salinidad",
                                                  "Nitritos","Nitratos","Oxigeno_Disuelto","Temperatura","Nut_Amonio")))[, variable := factor(variable, labels = c("Nut_Ortofosfatos","Porcentaje_Saturacion_Oxigeno","Conductividad","Salinidad",
                                                                                                                                                                   "Nitritos","Nitratos","Oxigeno_Disuelto","Temperatura","Nut_Amonio"))]
datos_wide$imp <- "SIN IMPUTACION"

#mergeo los 4 datasets
dataset <- rbind(datos_wide,longer_sample,longer_cart, longer_norm)

# CURVAS DE DENSIDAD PARA LA VISUALIZACION DE LAS IMPUTACIONES

ploty <- Rmisc::multiplot((ggplot(dataset[dataset$variable == "Conductividad",], aes(x=value, group = imp, color = imp)) + geom_density() + labs(title="Conductividad")),
                          (ggplot(dataset[dataset$variable == "Salinidad",], aes(x=value, group = imp, color = imp)) + geom_density() + labs(title="Salinidad")),
                          (ggplot(dataset[dataset$variable == "Nitritos",], aes(x=value, group = imp, color = imp)) + geom_density() + labs(title="Nitritos")),
                          (ggplot(dataset[dataset$variable == "Nitratos",], aes(x=value, group = imp, color = imp)) + geom_density() + labs(title="Nitratos")),
                          (ggplot(dataset[dataset$variable == "Temperatura",], aes(x=value, group = imp, color = imp)) + geom_density() + labs(title="Temperatura")),
                          (ggplot(dataset[dataset$variable == "Oxigeno_Disuelto",], aes(x=value, group = imp, color = imp)) + geom_density() + labs(title="Oxigeno_Disuelto")),
                          (ggplot(dataset[dataset$variable == "Porcentaje_Saturacion_Oxigeno",], aes(x=value, group = imp, color = imp)) + geom_density() + labs(title="Porcentaje_Saturacion_Oxigeno")),
                           cols = 3)

##########################################################################
#ANALISIS DE SENSIBILIDAD

#me quedo con los registros que estan 100% completos (sin faltantes)
prueba <- datosEj[complete.cases(datosEj),]

#Me armo un dataset de prueba con la misma cant de columnas que mi dataset de datos original

naVector <- sample(seq(5,13), size = nrow(prueba), replace = TRUE)

missingData <- function(dataset, naV){
  
  for(i in 1:nrow(dataset)){
    colNa <- naV[i]
    dataset[i, colNa] <- NA
  }
  return(dataset)
}

#"Borro" datos de mi dataset completo en base al naVector
df.na <- missingData(prueba, naVector)

test_sample <- get_data_imputed(setDF(df.na), "sample")
test_cart <- get_data_imputed(setDF(df.na), "cart")
test_norm <- get_data_imputed(setDF(df.na), "norm.nob")

#Al ejecutar el metodo norm.nob me daba el siguiente error:
# Error in solve.default(xtx + diag(pen)) : system is computationally singular: reciprocal condition number = 9.50431e-17
#elimine las columnas de coliformes y se dejó de dar



datos_sample <- fill_data_miss(setDF(df.na), test_sample$complete_data)
datos_cart <- fill_data_miss(setDF(df.na), test_cart$complete_data)
datos_norm <- fill_data_miss(setDF(df.na), test_norm$complete_data)

#comparacion entre valor real y el imputado

rmse.squares <- function(init.df, imputed.df, naV){
  
  result <- vector(mode = 'numeric', length = 0)
  
  for(i in 1:nrow(init.df)){
    colNa <- naV[i]
    rmse.result <- (imputed.df[i, colNa] - init.df[i, colNa])^2
    result <- append(result, rmse.result)

  }

  #agrego UNLIST aqui porque sino me devuelve una lista en vez de un vector.
    return(unlist(result, use.names=FALSE))
}



sample.squares <- rmse.squares(prueba, datos_sample, naVector)
cart.squares <- rmse.squares(prueba, datos_cart, naVector)
norm.squares <- rmse.squares(prueba, datos_norm, naVector)

rmse <- function(squares){
  result <- sqrt(sum(squares)/length(squares))  
  return(result)
}

sample.rmse <- rmse(sample.squares)
cart.rmse <- rmse(cart.squares)
norm.rmse <- rmse(norm.squares)
