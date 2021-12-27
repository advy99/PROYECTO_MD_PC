library(tidyverse)

source("funciones.r")

datos <- leer_datos("../data/training_set_features_preprocessed.csv", "../data/training_set_labels.csv", "../data/test_set_features_preprocessed.csv")

## Lectura de datos de entrenamiento
data <- datos[[1]]
labels <- datos[[2]]

summary(data)

## Lectura del conjunto de test
test <- datos[[3]]
summary(test)

training <- data.frame(data, labels)
summary(training)

## Extracción de las muestras
val <- random_sample(training, test, 0.8, 5)
summary(val[[1]])
summary(val[[2]])
