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


#
# Entrenamiento
# 

# Documentacion JRip en Weka: https://weka.sourceforge.io/doc.dev/weka/classifiers/rules/JRip.html
# Documentacion JRip en RWeka: https://cran.r-project.org/web/packages/RWeka/RWeka.pdf



train_JRip <- function(formula, datos, num_cv = 10, tune_grid = NULL) {
	
	# creamos el control de training con validación cruzada
	fit_control <- trainControl(method="cv", number = num_cv, verboseIter = T)
	
	# entrenamos el modelo
	modelo_JRip_entrenado <- train(formula, 
								  data = datos,
								  method = "JRip", 
								  metric = "Accuracy",
								  trControl = fit_control,
								  tuneGrid = tune_grid)
	
	# devolvemos el modelo de JRip
	modelo_JRip_entrenado
	
}

# TODO: Tune grid de parametros de JRip, aunque no tiene muchos

training_con_h1n1 <- base::cbind(training, training_labels[,1])

modelo_JRip_h1n1 <- train_JRip(h1n1_vaccine ~ ., training_con_h1n1, num_cv = 10)

predicciones_h1n1_vaccine_test <- predict(modelo_JRip_h1n1, test, type = "prob")


training_con_seasonal <- base::cbind(training, training_labels[,2])

modelo_JRip_seasonal <- train_JRip(seasonal_vaccine ~ ., training_con_seasonal, num_cv = 10)

predicciones_seasonal_vaccine_test <- predict(modelo_JRip_seasonal, test, type = "prob")


#
# Almacenamos resultados
#

resultados_predicciones_por_separado <- data.frame(respondent_id = c(26707:53414), 
												   h1n1_vaccine = predicciones_h1n1_vaccine_test$`1`, 
												   seasonal_vaccine = predicciones_seasonal_vaccine_test$`1`)

#Lo exportamos a CSV
write.csv(resultados,"../results/JRip_por_separado_sin_tunegrid_results.csv", row.names = F) 
