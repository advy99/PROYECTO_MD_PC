library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(RWeka)
source("funciones.r")

#########################
#   PREPROCESAMIENTO    #
#########################

#Vamos a comenzar leyendo los datos
x_train = read_csv("../data/training_set_features_preprocessed.csv")
x_test = read_csv("../data/test_set_features_preprocessed.csv")
y_train = read_csv("../data/training_set_labels.csv")

#Mostramos la estructura de train y test
str(x_train)
str(x_test)
str(y_train)

#Necesitamos pasar las variables de los ficheros de caracteristicas a su
#tipo correcto.
#Primero vamos a convertir a factor ordenado aquellas variables que 
#responden a preguntas de la encuesta con varios niveles.
x_train = x_train %>% mutate(across(c(1,2,16:23,29), as.ordered))
x_test = x_test %>% mutate(across(c(1,2,16:23,29), as.ordered))

#El resto de variables, menos household_adults y household_children las pasamos
#a factor sin orden.
x_train = x_train %>% mutate(across(-c(1,2,16:23,29,27,28), as.factor))
x_test = x_test %>% mutate(across(-c(1,2,16:23,29,27,28), as.factor))

#Vemos si los niveles de los factores estan en un orden correcto.
x_train %>% sapply(levels)
x_test %>% sapply(levels)

#En el caso de la variable "education" debemos darle prioridad a College
#graduate antes que some college.
levels(x_train$education) = c("< 12 Years","12 Years","Some College","College Graduate")
levels(x_test$education) = c("< 12 Years","12 Years","Some College","College Graduate")

#En el caso de la variable "income_poverty" debemos poner el nivel "Below Poverty"
#el primero de todos porque es el que indica un mayor nivel de pobreza.
levels(x_train$income_poverty) = c("Below Poverty","<= $75,000, Above Poverty",
                                   "> $75,000")
levels(x_test$income_poverty) = c("Below Poverty","<= $75,000, Above Poverty",
                                  "> $75,000")

#Pasamos a factor las dos variables objetivo de y_train
y_train = y_train %>% mutate(across(c(2,3), as.factor))


######################
#   ENTRENAMIENTO    #
######################

#Vamos a crear un custom random forest. Comenzaremos con uno para predecir
#la variable h1n1_vaccine.

x_train = cbind(x_train, y_train[,2])

salidas = data.frame(n = c(1:nrow(x_test)))

for (n in c(1:1000)){
  
  #Creamos las muestras para el arbol actual
  
  rows_percentage = runif(min=0,max=1,n=1)
  features = sample((1:33),1)

  muestras = random_sample(x_train,x_test,rows_percentage,features,"h1n1_vaccine")
  
  #Creamos un arbol con las muestras anteriores y lo entrenamos
  
  arbol = J48(h1n1_vaccine~., data=muestras[[1]],control=Weka_control(C=0.1,M=70))
  
  #Realizamos la prediccion del arbol y almacenamos los resultados
  
  predicted = predict(arbol, muestras[[2]], type = "prob")
  salidas = cbind(salidas,predicted[,2])
}

#Nos quedamos con la media de cada fila del dataframe de salidas.
colnames(salidas) = c("n",1:1000)
salidas = salidas %>% select(-1)
resultado_h1n1 = rowMeans(salidas)

#Hacemos lo mismo para predecir la variable seasonal_vaccine

x_train$h1n1_vaccine = NULL
x_train = base::cbind(x_train, y_train[,3])


salidas = data.frame(n = c(1:nrow(x_test)))

for (n in c(1:1000)){
  
  #Creamos las muestras para el arbol actual
  
  rows_percentage = runif(min=0,max=1,n=1)
  features = sample((1:33),1)
  
  muestras = random_sample(x_train,x_test,rows_percentage,features,"seasonal_vaccine")
  
  #Creamos un arbol con las muestras anteriores y lo entrenamos
  
  arbol = J48(seasonal_vaccine~., data=muestras[[1]],control=Weka_control(C=0.05,M=70))
  
  #Realizamos la prediccion del arbol y almacenamos los resultados
  
  predicted = predict(arbol, muestras[[2]], type = "prob")
  salidas = cbind(salidas,predicted[,2])
}

#Nos quedamos con la media de cada fila del dataframe de salidas.
colnames(salidas) = c("n",1:1000)
salidas = salidas %>% select(-1)
resultado_seasonal = rowMeans(salidas)


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
