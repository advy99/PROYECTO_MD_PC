library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(caret)

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


######################
#   ENTRENAMIENTO    #
######################

#Añadimos la variable objetivo h1n1_vaccine a x_train
x_train = base::cbind(x_train, y_train[,2])

rfGrid = expand.grid(.C=c(0.1,0.05,0.025,0.01,0.0075,0.005),
                     .M=c(1:5,seq(10,30,by=5),seq(40,100,by=10)))
fitControl = trainControl(method="cv", number = 10, verboseIter = T)

trained_c45_h1n1_vaccine = train(h1n1_vaccine ~ ., data=x_train,
                                 method="J48", metric="Accuracy",trControl=fitControl,
                                 tuneGrid=rfGrid)

#Los mejores parámetros encontrados han sido C=0.1 y M=70 con una accuracy en
#training de 0.8280976.

#Obtenemos las probabilidades del test set
prob_prediction_h1n1_vaccine = predict(trained_c45_h1n1_vaccine,x_test,type="prob")

#Ahora hacemos lo mismo para seasonal_vaccine 
#Añadimos la variable objetivo seasonal_vaccine a x_train
x_train$h1n1_vaccine = NULL
x_train = base::cbind(x_train, y_train[,3])

trained_c45_seasonal_vaccine = train(seasonal_vaccine ~ ., data=x_train,
                                 method="J48", metric="Accuracy",trControl=fitControl,
                                 tuneGrid=rfGrid)

#Los mejores parámetros encontrados han sido C=0.05 y M=70 con una accuracy en
#training de 0.7651931.

#Obtenemos las probabilidades del test set
prob_prediction_seasonal_vaccine = predict(trained_c45_seasonal_vaccine,x_test,type="prob")


###################
#   RESULTADOS    #
###################

#Creamos un dataframe con las probabilidades obtenidas. Este lo exportaremos
#a CSV y lo subiremos a DrivenData.

resultados = data.frame(respondent_id = c(26707:53414), h1n1_vaccine = 
                          prob_prediction_h1n1_vaccine$`1`, seasonal_vaccine = 
                          prob_prediction_seasonal_vaccine$`1`)

#Lo exportamos a CSV
write.csv(resultados,"../results/C45_J48_results.csv",row.names = F) 