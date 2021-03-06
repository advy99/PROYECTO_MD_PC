library(readr)
library(tidyverse)



### Funcion para extraer un subconjunto de caracteristicas e intancias para un modelo de
### ensemble tipo Random Forest.
random_sample <- function(training, test, train_percentage, num_features, 
                          features_range, target){
    ## Seleccionamos las caracteristicas a usar
    features <- sample(features_range, num_features, replace = F)

    ## Usamos los nombres para evitar problemas si test y train no siguen el mismo orden 
    features <- colnames(training)[features]


    ## Aniadimos las etiquetas a train.
    #Es necesario a?adir solamente la etiqueta que queremos predecir para que no
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


# Funci??n para leer los datos del problema aplicando las transformaciones necesarias
# a factores y si es necesario juntar las etiquetas
leer_datos <- function(ruta_predictores_train, ruta_etiquetas_train, ruta_predictores_test, juntar_etiquetas = FALSE,
					   factores_ordenados = c(1,2,16:23,26), factores = c(3:15, 24:25, 29:33)) {
	
	# leemos los datos de train
	x_train <- read_csv(ruta_predictores_train)
	x_test <- read_csv(ruta_predictores_test)
	
	y_train <- read_csv(ruta_etiquetas_train)
	
	
	#convertimos a factor ordenado las preguntas que las respuestas tienen cierto orden
	x_train <- x_train %>% mutate(across(factores_ordenados, as.ordered))
	x_test <- x_test %>% mutate(across(factores_ordenados, as.ordered))
	
	#El resto de variables, menos household_adults y household_children las pasamos
	#a factor sin orden.
	x_train <- x_train %>% mutate(across(factores, as.factor))
	x_test <- x_test %>% mutate(across(factores, as.factor))
	
	if ("education" %in% colnames(x_train)) {
		# le damos niveles de prioridad a la variable educacion
		levels(x_train$education) = c("< 12 Years","12 Years","Some College","College Graduate")
		levels(x_test$education) = c("< 12 Years","12 Years","Some College","College Graduate")
		
	}
	
	if ("income_poverty" %in% colnames(x_train)) {
		# le damos niveles de prioridad a los ingresos
		levels(x_train$income_poverty) = c("Below Poverty","<= $75,000, Above Poverty",
										   "> $75,000")
		levels(x_test$income_poverty) = c("Below Poverty","<= $75,000, Above Poverty",
										  "> $75,000")
	}
	

	
	# eliminamos el ID de las etiquetas de train
	y_train = y_train %>% select(-1)
	
	# el juntarlas es opcional, por si queremos hacer pruebas de predecir
	# una sola variable
	if (juntar_etiquetas) {
		# juntamos las variables a predecir en una sola columna
		y_train <- tidyr::unite(y_train, Y, c(h1n1_vaccine,seasonal_vaccine), sep="", remove = TRUE)
		
	}
	
	# cambiamos todas a factor
	y_train <- y_train %>% mutate_all(as.factor)
	
	
	list(x_train, y_train, x_test)
}
