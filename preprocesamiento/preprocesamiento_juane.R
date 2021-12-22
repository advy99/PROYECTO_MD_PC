library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(naniar)
library(Amelia)
library(caret)
library(mice)
library(FSelectorRcpp)

#Leemos las features y las labels de entrenamiento
x_train = read_csv("../data/training_set_features.csv",na = c('?','', 'NA'))
x_test = read_csv("../data/test_set_features.csv",na = c('?','', 'NA'))
y_train = read_csv("../data/training_set_labels.csv",na = c('?','', 'NA'))

#Vemos el tamanio de los sets
dim(x_train)
dim(y_train)
dim(x_test)

#Vemos la estructura de las columnas
str(x_train)

#A la vista de las variables de las que disponemos necesitariamos hacer lo
#siguiente:
# - Pasar todas las variables a factor
#
# - Buscar y lidiar con las variables con muchos NA.
#
# - Como varias variables son respuestas a encuestas de multiples opciones,
#   mirar si hay respuestas fuera de los limites y eliminarlas o cambiarlas
#   por otro valor en caso de que haya demasiadas.
#
# - Inputar el resto de missing values usando diferentes modelos predictivos.
#   Uno para las binarias, otro para las categoricas con orden y otro para las
#   categoricas sin orden.

#Eliminamos la primera variable (es un ID)
x_train = x_train %>% select(-1)
x_test = x_test %>% select(-1)

#Antes de pasar las variables a factor vamos a ver si los valores unicos de
#cada variable son de verdad "unicos", es decir, no existen variaciones del
#mismo valor cambiando mayusculas o cosas del estilo.
apply(x_train %>% select_if(is.character),2,unique)
apply(x_test %>% select_if(is.character),2,unique)

#Primero vamos a convertir a factor ordenado aquellas variables que 
#responden a preguntas de la encuesta con varios niveles.
x_train = x_train %>% mutate(across(c(1,2,16:23,26), as.ordered))
x_test = x_test %>% mutate(across(c(1,2,16:23,26), as.ordered))

#El resto de variables, menos household_adults y household_children las pasamos
#a factor sin orden.
x_train = x_train %>% mutate(across(-c(1,2,16:23,26,32,33), as.factor))
x_test = x_test %>% mutate(across(-c(1,2,16:23,26,32,33), as.factor))

#Vamos a ver las columnas en las que tenemos missing values
apply(x_train,2,function(x) any(is.na(x)))
missmap(x_train)

apply(x_test,2,function(x) any(is.na(x)))
missmap(x_test)

#La mayoria de las variables tienen NA, vamos a ver el ratio de missing values
#por cada variable
ratio_nulos = colSums(is.na(x_train))/nrow(x_train)
ratio_nulos[ratio_nulos > 0.3]

ratio_nulos_test = colSums(is.na(x_test))/nrow(x_test)
ratio_nulos_test[ratio_nulos_test > 0.3]

gg_miss_upset(x_train)
gg_miss_upset(x_test)

#Vemos que las variables health_insurance, employment_industry y 
#emplymentr_occupation tienen aproximadamente un 50% de los datos
#perdidos. Vamos a eliminarlas las dos últimas.
#Health_insurance la mantenemos porque el tener seguro médico o no lo 
#considero un factor importante a la hora de decidir si te vas a poner una
#vacuna o no. En EEUU sin seguro tienes que costearte la vacuna tu solo.
x_train = x_train %>% select(-c(employment_industry,employment_occupation))
x_test = x_test %>% select(-c(employment_industry,employment_occupation))

#Para eliminar el resto de missing values habria que usar imputacion.
#Vamos a usar diferentes métodos de imputación dependiendo de si la variable
#es factor sin orden, con orden o numerica (2 ultimas).
#Antes de imputar vamos a comprobar que los niveles de los factores son
#correctos.
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

#Una vez hemos comprobado que todos los niveles son correctos vamos a pasar
#a imputar los valores perdidos. Vamos a usar el metodo polr (Proportional
#adds model) para imputar los factores ordenados. Regresion logistica para
#imputar los valores booleanos. polyreg para imputar los factores no ordenados
#con mas de 2 niveles. pmm para las dos últimas variables numéricas.

###########################################
# DESCOMENTAR PARA IMPUTAR MISSING VALUES #
###########################################

#columns_with_missing = as.vector(which(apply(x_train,2,function(x) any(is.na(x))) == T))
#methods = c(rep("polr",2),rep("logreg",13),rep("polr",8),rep("polyreg",3),rep("pmm",2))
#imp = mice(x_train[,columns_with_missing], meth=methods, seed=1)
#x_train_sin_nulos = complete(imp)

#columns_with_missing = as.vector(which(apply(x_test,2,function(x) any(is.na(x))) == T))
#methods = c(rep("polr",2),rep("logreg",13),rep("polr",8),rep("polyreg",3),rep("pmm",2))
#imp = mice(x_test[,columns_with_missing], meth=methods, seed=1)
#x_test_sin_nulos = complete(imp)

x_train_sin_nulos = read_csv("../data/training_set_features_preprocessed.csv")
x_test_sin_nulos = read_csv("../data/test_set_features_preprocessed.csv")

#Vamos a usar FSelectorRcpp para ver las variables mas importantes.
#Primero eliminamos la columna ID de las etiquetas y juntamos 
#las dos otras columnas en una.

y_train = y_train %>% select(-1)

#Juntamos h1n1_vaccine y seasonal_vaccine en una sola columna y convertimos
#el resultado a factor
y_train = tidyr::unite(y_train,Y,c(h1n1_vaccine,seasonal_vaccine),sep="",remove=T)
y_train = y_train %>% mutate_all(as.factor)

pesosG = FSelectorRcpp::information_gain(Y ~ ., 
                                         base::cbind(x_train_sin_nulos,
                                                                y_train))

visual = head(pesosG %>% arrange(desc(importance)), 10)
ggplot(data=visual, aes(x=reorder(attributes, -importance), y=importance)) +
  geom_bar(fill="cornflowerblue", stat="identity")


#Falta mirar outliers y ruido
