library(tidyverse)

source("funciones.r")

## Lectura de datos de entrenamiento
data <- read.csv("../data/training_set_features.csv") %>%
    mutate(across(c(-1), as.factor))
labels <- read_csv("../data/training_set_labels.csv") %>%
    mutate(across(c(-1), as.factor))
training <- merge(x=data,y=labels,by.x="X",by.y = "respondent_id")
summary(training)

## Lectura del conjunto de test
test <- read_csv("../data/test_set_features.csv") %>%
    mutate(across(c(-1), as.factor))
summary(test)

## Extracción de las muestras
val <- random_sample(training, test, 0.8, 5)
summary(val[[1]])
summary(val[[2]])
