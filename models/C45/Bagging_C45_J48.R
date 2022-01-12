library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(RWeka)
source("../funciones.r")

#################
#   FUNCIONES   #
#################

CustomBaggingClassifier = function(train,test,n_trees,features_interval,
                                   target_variable,my_C,my_M,unite=F){
  
  salidas = data.frame(n = c(1:nrow(test)))
  salida_00 = data.frame(n = c(1:nrow(test)))
  salida_01 = data.frame(n = c(1:nrow(test)))
  salida_10 = data.frame(n = c(1:nrow(test)))
  salida_11 = data.frame(n = c(1:nrow(test)))
  train = cbind(train, target_variable)
  
  for (n in c(1:n_trees)){
    
    #Creamos las muestras para el arbol actual
    
    rows_percentage = runif(min=0,max=1,n=1)
    features = sample((features_interval),1)
    
    muestras = random_sample(train,test,rows_percentage,features,features_interval,
                             colnames(target_variable))
    
    #Creamos un arbol con las muestras anteriores y lo entrenamos
    
    arbol = J48(as.formula(paste(colnames(target_variable), "~.",collapse = "+")),
                data=muestras[[1]],control=Weka_control(C=my_C,M=my_M))
    
    #Realizamos la prediccion del arbol y almacenamos los resultados
    
    predicted = predict(arbol, muestras[[2]], type = "prob")
    
    if (unite == T){
      salida_00 = cbind(salida_00,predicted[,1])
      salida_01 = cbind(salida_01,predicted[,2])
      salida_10 = cbind(salida_10,predicted[,3])
      salida_11 = cbind(salida_11,predicted[,4])
      
    }
    else{
      salidas = cbind(salidas,predicted[,2])
    }
  }
  
  #Nos quedamos con la media de cada fila del dataframe de salidas.
  if (unite == T){
    colnames(salida_00) = c("n",1:n_trees)
    salida_00 = salida_00 %>% select(-1)
    colnames(salida_01) = c("n",1:n_trees)
    salida_01 = salida_01 %>% select(-1)
    colnames(salida_10) = c("n",1:n_trees)
    salida_10 = salida_10 %>% select(-1)
    colnames(salida_11) = c("n",1:n_trees)
    salida_11 = salida_11 %>% select(-1)
    
    resultado = rowMeans(salida_00)
    resultado = cbind(resultado,rowMeans(salida_01))
    resultado = cbind(resultado,rowMeans(salida_10))
    resultado = cbind(resultado,rowMeans(salida_11))
  }
  else{
    colnames(salidas) = c("n",1:n_trees)
    salidas = salidas %>% select(-1)
    resultado = rowMeans(salidas)   
  }
  resultado
}


#########################
#   PREPROCESAMIENTO    #
#########################

#Vamos a comenzar leyendo los datos
datos = leer_datos("../../data/training_set_features_preprocessed.csv", 
                   "../../data/training_set_labels.csv", 
                   "../../data/test_set_features_preprocessed.csv", juntar_etiquetas = FALSE,
                   factores_ordenados = c(1,2,16:23,29), factores = c(3:15,24:26,30:33))

x_train = datos[[1]]
y_train = datos[[2]]
x_test = datos[[3]]


#Mostramos la estructura de train y test
str(x_train)
str(x_test)
str(y_train)


######################
#   ENTRENAMIENTO    #
######################

#--------------------- 500 arboles --------------------#

#Vamos a crear un custom bagging. Comenzaremos con uno para predecir
#la variable h1n1_vaccine.

resultado_h1n1_500 = CustomBaggingClassifier(x_train,x_test,500,1:33,
                                         y_train[,1],0.15,40)

#Hacemos lo mismo para predecir la variable seasonal_vaccine

resultado_seasonal_500 = CustomBaggingClassifier(x_train,x_test,500,1:33,
                                         y_train[,2],0.025,25)

#--------------------- 1000 arboles --------------------#

#Vamos a crear un custom bagging. Comenzaremos con uno para predecir
#la variable h1n1_vaccine.

resultado_h1n1_1000 = CustomBaggingClassifier(x_train,x_test,1000,1:33,
                                             y_train[,1],0.15,40)

#Hacemos lo mismo para predecir la variable seasonal_vaccine

resultado_seasonal_1000 = CustomBaggingClassifier(x_train,x_test,1000,1:33,
                                                 y_train[,2],0.025,25)


#--------------------- Juntando variables 1000 arboles --------------------#

#Vamos a probar a juntar las dos variables objetivo en una y predecirlas juntas
y_train = tidyr::unite(y_train, Y, c(h1n1_vaccine,seasonal_vaccine), sep="", remove = TRUE)
y_train$Y = as.factor(y_train$Y)
resultado_h1n1_seasonal = CustomBaggingClassifier(x_train,x_test,1000,
                                                 1:33,y_train[,1],0.2,80,TRUE)
colnames(resultado_h1n1_seasonal) = c("00","01","10","11")
resultado_h1n1_seasonal = as.data.frame(resultado_h1n1_seasonal)

###################
#   RESULTADOS    #
###################

#Creamos un dataframe con las probabilidades obtenidas. Este lo exportaremos
#a CSV y lo subiremos a DrivenData.

resultados_500 = data.frame(respondent_id = c(26707:53414), h1n1_vaccine = 
                          unname(resultado_h1n1_500), seasonal_vaccine = 
                          unname(resultado_seasonal_500))

#Lo exportamos a CSV
write.csv(resultados_500,"../../results/Bagging_C45_J48_results_500.csv",row.names = F)  


#Guardamos los resultados con 1000 arboles

resultados_1000 = data.frame(respondent_id = c(26707:53414), h1n1_vaccine = 
                              unname(resultado_h1n1_1000), seasonal_vaccine = 
                              unname(resultado_seasonal_1000))

#Lo exportamos a CSV
write.csv(resultados_1000,"../../results/Bagging_C45_J48_results_1000.csv",row.names = F)  


#Ahora con los resultados obtenidos al predecir ambas variables objetivo
#juntas

resultados_juntas = data.frame(respondent_id = c(26707:53414), h1n1_vaccine = 
                                 resultado_h1n1_seasonal$`10` + resultado_h1n1_seasonal$`11`,
                               seasonal_vaccine = resultado_h1n1_seasonal$`01` + resultado_h1n1_seasonal$`11`)


#Lo exportamos a CSV
write.csv(resultados_juntas,"../../results/Bagging_C45_J48_results_united.csv",row.names = F)  
