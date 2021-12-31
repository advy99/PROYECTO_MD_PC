library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(RWeka)
source("funciones.r")

#################
#   FUNCIONES   #
#################

RandomForestClassifier = function(train,test,n_trees,rows_percentage,features,
                                  features_interval,target_variable){
  
  salidas = data.frame(n = c(1:nrow(test)))
  train = cbind(train, target_variable)
  
  for (n in c(1:n_trees)){
    
    #Creamos las muestras para el arbol actual
    
    muestras = random_sample(train,test,rows_percentage,features,features_interval,
                             colnames(target_variable))
    
    #Creamos un arbol con las muestras anteriores y lo entrenamos
    
    arbol = J48(as.formula(paste(colnames(target_variable), "~.",collapse = "+")),
                data=muestras[[1]],control=Weka_control(C=0.1,M=70))
    
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
                   factores_ordenados = c(1,2,16:23,29), factores = -c(1,2,16:23,29,27,28))

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

#Vamos a crear un custom Random Forest. Comenzaremos con uno para predecir
#la variable h1n1_vaccine.

#Determinamos el número de variables aleatorias a usar por cada árbol como
#la raiz cuadrada del número total de variables.

features = as.integer(sqrt(ncol(x_train)))
rows_percentage = 1.0

#Llamamos a la funcion RandomForestClassifier

resultado_h1n1 = RandomForestClassifier(x_train,x_test,1000,rows_percentage,features,
  1:33, y_train[,1])

#Hacemos lo mismo para predecir la variable seasonal_vaccine

resultado_seasonal = RandomForestClassifier(x_train,x_test,1000,rows_percentage,features,
                                        1:33,y_train[,2])

###################
#   RESULTADOS    #
###################

#Creamos un dataframe con las probabilidades obtenidas. Este lo exportaremos
#a CSV y lo subiremos a DrivenData.

resultados = data.frame(respondent_id = c(26707:53414), h1n1_vaccine = 
                          unname(resultado_h1n1), seasonal_vaccine = 
                          unname(resultado_seasonal))

#Lo exportamos a CSV
write.csv(resultados,"../results/RF_C45_J48_results.csv",row.names = F)  