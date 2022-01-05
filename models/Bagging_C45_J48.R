library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(RWeka)
source("funciones.r")

#################
#   FUNCIONES   #
#################

CustomBaggingClassifier = function(train,test,n_trees,features_interval,
                                   target_variable,my_C,my_M){
  
  salidas = data.frame(n = c(1:nrow(test)))
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
    salidas = cbind(salidas,predicted[,2])
  }
  
  #Nos quedamos con la media de cada fila del dataframe de salidas.
  colnames(salidas) = c("n",1:n_trees)
  salidas = salidas %>% select(-1)
  resultado = rowMeans(salidas)  
  resultado
}


#########################
#   PREPROCESAMIENTO    #
#########################

#Vamos a comenzar leyendo los datos
datos = leer_datos("../data/training_set_features_preprocessed.csv", 
                   "../data/training_set_labels.csv", 
                   "../data/test_set_features_preprocessed.csv", juntar_etiquetas = FALSE,
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

#Vamos a crear un custom bagging. Comenzaremos con uno para predecir
#la variable h1n1_vaccine.

resultado_h1n1 = CustomBaggingClassifier(x_train,x_test,1000,1:33,
                                         y_train[,1],0.225,80)

#Hacemos lo mismo para predecir la variable seasonal_vaccine

resultado_seasonal = CustomBaggingClassifier(x_train,x_test,1000,1:33,
                                         y_train[,2],0.05,15)

###################
#   RESULTADOS    #
###################

#Creamos un dataframe con las probabilidades obtenidas. Este lo exportaremos
#a CSV y lo subiremos a DrivenData.

resultados = data.frame(respondent_id = c(26707:53414), h1n1_vaccine = 
                          unname(resultado_h1n1), seasonal_vaccine = 
                          unname(resultado_seasonal))

#Lo exportamos a CSV
write.csv(resultados,"../results/Bagging_C45_J48_results.csv",row.names = F)  
