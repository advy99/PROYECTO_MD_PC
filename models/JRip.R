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
	fit_control <- trainControl(method="cv", number = num_cv, verboseIter = T,
								classProbs = TRUE,
								summaryFunction = twoClassSummary)

	# entrenamos el modelo
	modelo_JRip_entrenado <- train(formula, 
								  data = datos,
								  method = "JRip", 
								  metric = "ROC",
								  trControl = fit_control,
								  tuneGrid = tune_grid)
	
	# devolvemos el modelo de JRip
	modelo_JRip_entrenado
	
}

# TODO: Tune grid de parametros de JRip, aunque no tiene muchos

training_con_h1n1 <- base::cbind(training, training_labels[,1])
levels(training_con_h1n1$h1n1_vaccine) <- c("X0", "X1")

# tune grid añadido a posteriori una vez tenemos los mejores hiperparámetros
tune_grid_h1n1 <- expand.grid(NumOpt = c(1), NumFolds = c(4), MinWeights = c(3))
modelo_JRip_h1n1 <- train_JRip(h1n1_vaccine ~ ., training_con_h1n1, num_cv = 10, tune_grid = tune_grid_h1n1)

#
# Mejor tune obtenido para h1n1_vaccine:
# Fitting NumOpt = 1, NumFolds = 4, MinWeights = 3 on full training set
#

predicciones_h1n1_vaccine_test <- predict(modelo_JRip_h1n1, test, type = "prob")

summary(modelo_JRip_h1n1)



training_con_seasonal <- base::cbind(training, training_labels[,2])
levels(training_con_seasonal$seasonal_vaccine) <- c("X0", "X1")

# tune grid añadido a posteriori una vez tenemos los mejores hiperparámetros
tune_grid_seasonal <- expand.grid(NumOpt = c(3), NumFolds = c(2), MinWeights = c(2))
modelo_JRip_seasonal <- train_JRip(seasonal_vaccine ~ ., training_con_seasonal, num_cv = 10, tune_grid = tune_grid_seasonal)

predicciones_seasonal_vaccine_test <- predict(modelo_JRip_seasonal, test, type = "prob")

#
# Mejor tune obtenido para seasonal_vaccine:
# Fitting NumOpt = 3, NumFolds = 2, MinWeights = 2 on full training set
#

summary(modelo_JRip_seasonal)


#
# Almacenamos resultados
#

resultados_predicciones_por_separado <- data.frame(respondent_id = c(26707:53414), 
												   h1n1_vaccine = predicciones_h1n1_vaccine_test$`X1`, 
												   seasonal_vaccine = predicciones_seasonal_vaccine_test$`X1`)

#Lo exportamos a CSV
write.csv(resultados_predicciones_por_separado, "../results/JRip_por_separado_metric_ROC_results.csv", row.names = F) 



# probamos a entrenar un modelo que utilice h1n1 para predecir seasonal vaccine

training_completo <- base::cbind(training_con_h1n1, training_labels[,2])
levels(training_completo$seasonal_vaccine) <- c("X0", "X1")

tune_grid_seasonal_completo <- expand.grid(NumOpt = c(3), NumFolds = c(3), MinWeights = c(3))
modelo_JRip_seasonal_completo <- train_JRip(seasonal_vaccine ~ ., training_completo, num_cv = 10, tune_grid = tune_grid_seasonal_completo)

test_con_predicciones_h1n1 <- base::cbind(test, h1n1_vaccine = ifelse(predicciones_h1n1_vaccine_test$`X1` < 0.5, 0, 1))
test_con_predicciones_h1n1$h1n1_vaccine <- as.factor(test_con_predicciones_h1n1$h1n1_vaccine)
levels(test_con_predicciones_h1n1$h1n1_vaccine) <- c("X0", "X1")


predicciones_seasonal_vaccine_test_completo <- predict(modelo_JRip_seasonal_completo, test_con_predicciones_h1n1, type = "prob")


resultados_predicciones_completo <- data.frame(respondent_id = c(26707:53414), 
												   h1n1_vaccine = predicciones_h1n1_vaccine_test$`X1`, 
												   seasonal_vaccine = predicciones_seasonal_vaccine_test_completo$`X1`)

#Lo exportamos a CSV
write.csv(resultados_predicciones_completo, "../results/JRip_metric_ROC_h1n1_predictor_results.csv", row.names = F) 
