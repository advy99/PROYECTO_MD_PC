library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(caret)

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