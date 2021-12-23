source(funciones.r)

library(RWeka)
library(caret)

#
# Lectura de datos
#

datos <- leer_datos("../data/training_set_features_preprocessed.csv", 
					"../data/training_set_labels.csv", 
					"../data/test_set_features_preprocessed.csv")

training <- datos[[1]]
training_labels <- datos[[2]]

test <- datos[[3]]


