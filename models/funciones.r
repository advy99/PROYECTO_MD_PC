
### Funcion para extraer un subconjunto de caracteristicas e intancias para un modelo de
### ensemble tipo Random Forest.
random_sample <- function(training, test, train_percentage, num_features){
    ## Seleccionamos las caracteristicas a usar
    features <- sample(2:29, num_features)

    ## Usamos los nombres para evitar problemas si test y train no siguen el mismo orden 
    features <- colnames(training)[features]

    ## Añadimos las etiquetas a train
    training_features <- c(features,c("h1n1_vaccine","seasonal_vaccine"))

    ## Seleccionamos columnas aleatoriamente
    rows <- sample(1:nrow(training),nrow(training)*train_percentage)

    ## Extraemos las muestras en ambos conjuntos
    train_sample <- training[rows,training_features]

    test_sample <- test[,features]

    ## Devolvemos ambas muestras
    return(list(train_sample,test_sample))
}
