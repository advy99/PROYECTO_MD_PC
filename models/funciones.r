
### Funcion para extraer un subconjunto de caracteristicas e intancias para un modelo de
### ensemble tipo Random Forest.
random_sample <- function(training, test, train_percentage, num_features, 
                          features_range, target){
    ## Seleccionamos las caracteristicas a usar
    features <- sample(features_range, num_features, replace = F)

    ## Usamos los nombres para evitar problemas si test y train no siguen el mismo orden 
    features <- colnames(training)[features]

    ## AÃ±adimos las etiquetas a train.
    #Es necesario añadir solamente la etiqueta que queremos predecir para que no
    #haya inconsistencias a la hora de predecir en test.
    training_features <- c(features,target)

    ## Seleccionamos columnas aleatoriamente
    rows <- sample(1:nrow(training),nrow(training)*train_percentage, replace = T)

    ## Extraemos las muestras en ambos conjuntos
    train_sample <- training[rows,training_features]

    test_sample <- test[,features]

    ## Devolvemos ambas muestras
    return(list(train_sample,test_sample))
}
