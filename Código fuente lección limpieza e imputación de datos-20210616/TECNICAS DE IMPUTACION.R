
# TECNICAS PARA LA IMPUTACION DE DATOS

# DATA.TABLE, MISSING DATA ANALYSIS AND IMPUTATION

packages <- c("data.table", "naniar", "mice", "dplyr", "Rmisc", "readxl", "ggplot2")

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

ipak(packages)

# PACKAGE NANIAR

datos <- readxl::read_xlsx("C:\\Users\\camil\\icam_datos.xlsx")
datos <- subset(datos, select = c("DEPTO","CODEST","MUESTREO","SUSTRATO",
                                  "CA_CLA","CA_CTE","CA_DBO","CA_HDD","CA_NO3","CA_OD","CA_PH","CA_PO4","CA_SST"))
datos <- datos[with(datos, order(DEPTO,MUESTREO,SUSTRATO,CODEST)),]

# ADD_ANY_MISS - AÑADE UNA COLUMNA AL DATAFRAME INDICANDO SI EN LA FILA HAY O NO HAY DATOS FALTANTES

test <- datos %>% add_any_miss()
test <- datos %>% add_any_miss(CA_HDD, CA_NO3)

# ADD_MISS_CLUSTER - CREA UNA COLUMNA INDICANDO EL CLUSTER AL QUE PERTENECE LA FILA SEGUN LOS DATOS FALTANTES

test <- add_miss_cluster(datos, n_clusters = 4)

# NUMERO DE FALTANTES POR FILA

test <- add_n_miss(datos, label = "n_miss")

# HISTOGRAMA DE CASOS FALTANTES

naniar::gg_miss_upset(datos, nsets = 9)

# NUMERO DE FALTANTES POR VARIABLE

gg_miss_var(datos, show_pct = TRUE) + ylim(0, 100)
gg_miss_var(datos, DEPTO, show_pct = TRUE) + ylim(0, 100)

# PORCENTAJES

test <- datos
test$prcNa <- prop_miss_row(datos)

# EJERCICIO DE IMPUTACION 

# TRATAMIENTO DE DATOS FALTANTES

# VISUALIZACION DE DATOS FALTANTES

gg_miss_fct(datos[,4:13], fct = SUSTRATO) +
  theme(axis.text.x = element_text(size = 12, angle=0, hjust=0.5), axis.title.x=element_blank(), axis.title.y=element_blank())

gg_miss_upset(datos[,5:13])

# DISTRIBUCION NA

density_na <- multiplot(ggplot(data = datos[!complete.cases(datos),4:13], aes(CA_PH, color = SUSTRATO)) + geom_density() + theme_classic(),
                        ggplot(data = datos[!complete.cases(datos),4:13], aes(CA_PO4, color = SUSTRATO)) + geom_density() + theme_classic(),
                        ggplot(data = datos[!complete.cases(datos),4:13], aes(CA_SST, color = SUSTRATO)) + geom_density() + theme_classic(),
                        ggplot(data = datos[!complete.cases(datos),4:13], aes(CA_NO3, color = SUSTRATO)) + geom_density() + theme_classic(),
                        ggplot(data = datos[!complete.cases(datos),4:13], aes(CA_OD, color = SUSTRATO)) + geom_density() + theme_classic(),
                        ggplot(data = datos[!complete.cases(datos),4:13], aes(CA_HDD, color = SUSTRATO)) + geom_density() + theme_classic(),
                        ggplot(data = datos[!complete.cases(datos),4:13], aes(CA_DBO, color = SUSTRATO)) + geom_density() + theme_classic(),
                        ggplot(data = datos[!complete.cases(datos),4:13], aes(CA_CTE, color = SUSTRATO)) + geom_density() + theme_classic(),
                        ggplot(data = datos[!complete.cases(datos),4:13], aes(CA_CLA, color = SUSTRATO)) + geom_density() + theme_classic(),  
                        cols = 3)

# IMPUTACIONES

# IMPUTACION DE LA MEDIA 

impute_mean_df <- function(x){
  
  x <- x %>% group_by(CODEST,SUSTRATO,variable) %>% impute_mean_if(.predicate = is.numeric)
  return(x)
}

impute_median_df <- function(x){
  
  x <- x %>% group_by(CODEST,SUSTRATO,variable) %>% impute_median_if(.predicate = is.numeric)
  return(x)
}

datos_wide <- melt(setDT(datos), id.vars = c("DEPTO","CODEST","MUESTREO","SUSTRATO"),
                   measure.vars = list(value = c("CA_CLA","CA_CTE","CA_DBO","CA_HDD","CA_NO3","CA_OD","CA_PH","CA_PO4","CA_SST")))[, variable := factor(variable, labels = c("CA_CLA","CA_CTE","CA_DBO","CA_HDD","CA_NO3","CA_OD","CA_PH","CA_PO4","CA_SST"))]

datos_wide$imp <- "SIN IMPUTACION"
imputed_1 <- impute_mean_df(datos_wide) %>% mutate(imp = "MEDIA")
imputed_2 <- impute_median_df(datos_wide) %>% mutate(imp = "MEDIANA")

dataset <- rbind(datos_wide, data.frame(imputed_1), data.frame(imputed_2))

# CURVAS DE DENSIDAD PARA LA VISUALIZACION DE LAS IMPUTACIONES

ploty <- Rmisc::multiplot((ggplot(dataset[dataset$variable == "CA_OD",], aes(x=value, group = imp, color = imp)) + geom_density() + labs(title="CA_OD")),
                          (ggplot(dataset[dataset$variable == "CA_PH",], aes(x=value, group = imp, color = imp)) + geom_density() + labs(title="CA_PH")),
                          (ggplot(dataset[dataset$variable == "CA_NO3",], aes(x=value, group = imp, color = imp)) + geom_density() + labs(title="CA_NO3")),
                          (ggplot(dataset[dataset$variable == "CA_PO4",], aes(x=value, group = imp, color = imp)) + geom_density() + labs(title="CA_PO4")),
                          (ggplot(dataset[dataset$variable == "CA_SST",], aes(x=value, group = imp, color = imp)) + geom_density() + labs(title="CA_SST")),
                          (ggplot(dataset[dataset$variable == "CA_DBO",], aes(x=value, group = imp, color = imp)) + geom_density() + labs(title="CA_DBO")),
                          (ggplot(dataset[dataset$variable == "CA_HDD",], aes(x=value, group = imp, color = imp)) + geom_density() + labs(title="CA_HDD")),
                          (ggplot(dataset[dataset$variable == "CA_CTE",], aes(x=value, group = imp, color = imp)) + geom_density() + labs(title="CA_CTE")),
                          (ggplot(dataset[dataset$variable == "CA_CLA",], aes(x=value, group = imp, color = imp)) + geom_density() + labs(title="CA_CLA")),
                          cols = 3)

# MICE IMPUTATION

get_data_imputed <- function(x, methodx){
  
  imputed_data <- mice(x[,5:ncol(x)], m = 5, maxit = 5, method = methodx)
  d_plot <- densityplot(imputed_data)
  data_complete <- mice::complete(imputed_data)
  
  resultado <- list(imputed_data = imputed_data, plot = d_plot, complete_data = data_complete)
  
  return(resultado)
}

resultado_pmm <- get_data_imputed(datos, "pmm")
resultado_cart <- get_data_imputed(datos, "cart")

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

datos_pmm <- fill_data_miss(setDF(datos), resultado_pmm$complete_data)
datos_cart <- fill_data_miss(setDF(datos), resultado_cart$complete_data)

longer_pmm <- melt(setDT(datos_pmm), id.vars = c("DEPTO","CODEST","MUESTREO","SUSTRATO"),
                   measure.vars = list(value = c("CA_CLA","CA_CTE","CA_DBO","CA_HDD","CA_NO3","CA_OD","CA_PH","CA_PO4","CA_SST")))[, variable := factor(variable, labels = c("CA_CLA","CA_CTE","CA_DBO","CA_HDD","CA_NO3","CA_OD","CA_PH","CA_PO4","CA_SST"))]

longer_cart <- melt(setDT(datos_cart), id.vars = c("DEPTO","CODEST","MUESTREO","SUSTRATO"),
                    measure.vars = list(value = c("CA_CLA","CA_CTE","CA_DBO","CA_HDD","CA_NO3","CA_OD","CA_PH","CA_PO4","CA_SST")))[, variable := factor(variable, labels = c("CA_CLA","CA_CTE","CA_DBO","CA_HDD","CA_NO3","CA_OD","CA_PH","CA_PO4","CA_SST"))]

longer_pmm <- longer_pmm %>% mutate(imp = "pmm")
longer_cart <- longer_cart %>% mutate(imp = "cart")

dataset <- rbind(dataset,longer_pmm,longer_cart)

ploty <- Rmisc::multiplot((ggplot(dataset[dataset$variable == "CA_OD",], aes(x = value, group = imp, color = imp)) + geom_density() + labs(title="CA_OD")),
                          (ggplot(dataset[dataset$variable == "CA_PH",], aes(x = value, group = imp, color = imp)) + geom_density() + labs(title="CA_PH")),
                          (ggplot(dataset[dataset$variable == "CA_NO3",], aes(x = value, group = imp, color = imp)) + geom_density() + labs(title="CA_NO3")),
                          (ggplot(dataset[dataset$variable == "CA_PO4",], aes(x = value, group = imp, color = imp)) + geom_density() + labs(title="CA_PO4")),
                          (ggplot(dataset[dataset$variable == "CA_SST",], aes(x = value, group = imp, color = imp)) + geom_density() + labs(title="CA_SST")),
                          (ggplot(dataset[dataset$variable == "CA_DBO",], aes(x = value, group = imp, color = imp)) + geom_density() + labs(title="CA_DBO")),
                          (ggplot(dataset[dataset$variable == "CA_HDD",], aes(x = value, group = imp, color = imp)) + geom_density() + labs(title="CA_HDD")),
                          (ggplot(dataset[dataset$variable == "CA_CTE",], aes(x = value, group = imp, color = imp)) + geom_density() + labs(title="CA_CTE")),
                          (ggplot(dataset[dataset$variable == "CA_CLA",], aes(x = value, group = imp, color = imp)) + geom_density() + labs(title="CA_CLA")),
                          cols = 3)


# ANALISIS DE SENSIBILIDAD 

datos <- data.frame(read_xlsx("C:\\Users\\camil\\icam_datos.xlsx"))
datos <- subset(datos, select = c("DEPTO","CODEST","MUESTREO","SUSTRATO",
                                  "CA_CLA","CA_CTE","CA_DBO","CA_HDD","CA_NO3","CA_OD","CA_PH","CA_PO4","CA_SST"))

prueba <- datos[complete.cases(datos),]

naVector <- sample(seq(5,13), size = nrow(prueba), replace = TRUE)

missingData <- function(dataset, naV){
  
  for(i in 1:nrow(dataset)){
    colNa <- naV[i]
    dataset[i, colNa] <- NA
  }
  return(dataset)
}

df.na <- missingData(prueba, naVector)

pmm.result <- get_data_imputed(df.na, methodx = "pmm")
cart.result <- get_data_imputed(df.na, methodx = "cart")

datos_pmm <- fill_data_miss(df.na, pmm.result$complete_data)
datos_cart <- fill_data_miss(df.na, cart.result$complete_data)

library(imager)
joins <- load.image("C:\\Users\\camil\\Downloads\\rmse.jpg")
display(joins)

rmse.squares <- function(init.df, imputed.df, naV){
  
  result <- vector(mode = 'numeric', length = 0)
    
  for(i in 1:nrow(init.df)){
    colNa <- naV[i]
    rmse.result <- (imputed.df[i, colNa] - init.df[i, colNa])^2
    result <- append(result, rmse.result)
  }
  return(result)
}

pmm.squares <- rmse.squares(prueba, datos_pmm, naVector)
cart.squares <- rmse.squares(prueba, datos_cart, naVector)

rmse <- function(squares){
  result <- sqrt(sum(squares)/length(squares))  
  return(result)
}

pmm.rmse <- rmse(pmm.squares)
cart.rmse <- rmse(cart.squares)
