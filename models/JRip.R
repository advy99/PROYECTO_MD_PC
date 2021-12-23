source(funciones.r)

library(RWeka)
library(caret)

#
# Lectura de datos
#

datos <- leer_datos("../data/training_set_features_preprocessed.csv", 
					"../data/training_set_labels.csv", 
					"../data/test_set_features_preprocessed.csv")

training <- datos[[0]]
tarining_labels <- datos[[1]]

test <- datos[[2]]


