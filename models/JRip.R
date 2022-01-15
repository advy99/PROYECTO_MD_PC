source("funciones.r")

library(RWeka)
library(caret)

#
# Lectura de datos
#

datos <- leer_datos("../data/training_set_features_preprocessed.csv", 
					"../data/training_set_labels.csv", 
					"../data/test_set_features_preprocessed.csv", juntar_etiquetas = FALSE)

training <- datos[[1]]
training_labels <- datos[[2]]

test <- datos[[3]]


#
# Entrenamiento
# 

# Documentacion JRip en Weka: https://weka.sourceforge.io/doc.dev/weka/classifiers/rules/JRip.html
# Documentacion JRip en RWeka: https://cran.r-project.org/web/packages/RWeka/RWeka.pdf



train_JRip <- function(formula, datos, num_cv = 10, tune_grid = NULL, metrica = "Accuracy") {
	
	# creamos el control de training con validación cruzada
	fit_control <- trainControl(method="cv", number = num_cv, verboseIter = T)
	
	if (metrica == "ROC") {
		fit_control <- trainControl(method="cv", number = num_cv, verboseIter = T,
									classProbs = TRUE,
									summaryFunction = twoClassSummary)
	}

	# entrenamos el modelo
	modelo_JRip_entrenado <- train(formula, 
								  data = datos,
								  method = "JRip", 
								  metric = metrica,
								  trControl = fit_control,
								  tuneGrid = tune_grid)
	
	# devolvemos el modelo de JRip
	modelo_JRip_entrenado
	
}


bagging_JRip <- function(x_train, x_test, num_bootstrap, features_interval,
						 y_train, funcion_agregacion = "media"){
	
	salidas <- data.frame(n = c(1:nrow(x_test)))
	train <- cbind(x_train, y_train)
	
	for (n in c(1:num_bootstrap)){
		
		# para cada iteración creamos el conjunto de muestras
		rows_percentage <- runif(min=0.2,max=0.8,n=1)
		features <- sample((features_interval),1)
		
		muestras <- random_sample(train, test, rows_percentage, features,features_interval,
								  colnames(y_train))
		
		# Entrenamos JRip con las muestras anteriores
		
		jrip_instance <- JRip(as.formula(paste(colnames(y_train), "~.",collapse = "+")), 
							  data = muestras[[1]])
		
		# Predecimos el resultado y lo introducimos en las salidas
		
		predicted <- predict(jrip_instance, muestras[[2]], type = "prob")
		salidas <- cbind(salidas, predicted[,2])
		
		if (n %% 10 == 0) {
			cat(paste("Bootstrapped training data set number:", n, "\n"))
		}
		
		
	}
	
	# Nos quedamos con la media de cada fila del dataframe de salidas.
	colnames(salidas) <- c("n",1:num_bootstrap)
	salidas = salidas %>% select(-1)
	
	if (funcion_agregacion == "media") {
		resultado = rowMeans(salidas)
	} else if (funcion_agregacion == "mediana") {
		resultado = apply(salidas, 1, median, na.rm = T)
	}
	
	resultado
}


#
# Ejecucion basica
#

# preparamos
training_con_h1n1 <- base::cbind(training, training_labels[,1])

# Si usamos métrica ROC
#levels(training_con_h1n1$h1n1_vaccine) <- c("X0", "X1")

# tune grid añadido a posteriori una vez tenemos los mejores hiperparámetros
tune_grid_h1n1 <- expand.grid(NumOpt = c(1), NumFolds = c(4), MinWeights = c(3))
modelo_JRip_h1n1 <- train_JRip(h1n1_vaccine ~ ., training_con_h1n1, num_cv = 10, tune_grid = tune_grid_h1n1, metrica = "Accuracy")

#
# Mejor tune obtenido para h1n1_vaccine:
# Fitting NumOpt = 1, NumFolds = 4, MinWeights = 3 on full training set
#

predicciones_h1n1_vaccine_test <- predict(modelo_JRip_h1n1, test, type = "prob")

summary(modelo_JRip_h1n1)



training_con_seasonal <- base::cbind(training, training_labels[,2])

# Si usamos métrica ROC
#levels(training_con_seasonal$seasonal_vaccine) <- c("X0", "X1")

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
												   h1n1_vaccine = predicciones_h1n1_vaccine_test$`1`, 
												   seasonal_vaccine = predicciones_seasonal_vaccine_test$`1`)

#Lo exportamos a CSV
write.csv(resultados_predicciones_por_separado, "../results/JRip_por_separado_metric_ROC_results.csv", row.names = F) 



#
# Usamos la predicción de h1n1 para predecir seasonal vaccine
#


# probamos a entrenar un modelo que utilice h1n1 para predecir seasonal vaccine

training_completo <- base::cbind(training_con_h1n1, training_labels[,2])
# Si usamos métrica ROC
#levels(training_con_seasonal$seasonal_vaccine) <- c("X0", "X1")


tune_grid_seasonal_completo <- expand.grid(NumOpt = c(3), NumFolds = c(3), MinWeights = c(3))
modelo_JRip_seasonal_completo <- train_JRip(seasonal_vaccine ~ ., training_completo, num_cv = 10, tune_grid = tune_grid_seasonal_completo)

summary(modelo_JRip_seasonal_completo)

test_con_predicciones_h1n1 <- base::cbind(test, h1n1_vaccine = ifelse(predicciones_h1n1_vaccine_test$`X1` < 0.5, 0, 1))
test_con_predicciones_h1n1$h1n1_vaccine <- as.factor(test_con_predicciones_h1n1$h1n1_vaccine)


predicciones_seasonal_vaccine_test_completo <- predict(modelo_JRip_seasonal_completo, test_con_predicciones_h1n1, type = "prob")


resultados_predicciones_completo <- data.frame(respondent_id = c(26707:53414), 
												   h1n1_vaccine = predicciones_h1n1_vaccine_test$`1`, 
												   seasonal_vaccine = predicciones_seasonal_vaccine_test_completo$`1`)

#Lo exportamos a CSV
write.csv(resultados_predicciones_completo, "../results/JRip_metric_ROC_h1n1_predictor_results.csv", row.names = F) 






datos_unica_etiqueta <- leer_datos("../data/training_set_features_preprocessed.csv", 
					"../data/training_set_labels.csv", 
					"../data/test_set_features_preprocessed.csv", juntar_etiquetas = TRUE)

training_labels_una_etiqueta <- datos_unica_etiqueta[[2]]


# probamos a predecir ambas variables juntas

training_con_etiqueta <- base::cbind(training, training_labels_una_etiqueta[,1])
# Si usamos métrica ROC
#levels(training_con_etiqueta$Y) <- c("X0", "X1")


# tune grid añadido a posteriori una vez tenemos los mejores hiperparámetros
tune_grid_completo <- expand.grid(NumOpt = c(3), NumFolds = c(2), MinWeights = c(2))
modelo_JRip_completo <- train_JRip(Y ~ ., training_con_etiqueta, num_cv = 10, tune_grid = tune_grid_completo)

summary(modelo_JRip_completo)

predicciones_test <- predict(modelo_JRip_completo, test, type = "prob")


resultados_predicciones_completo <- data.frame(respondent_id = c(26707:53414), 
												   h1n1_vaccine = predicciones_test$`10` + predicciones_test$`11`, 
												   seasonal_vaccine = predicciones_test$`01` + predicciones_test$`11`)

write.csv(resultados_predicciones_completo, "../results/JRip_cuatro_clases_results.csv", row.names = F) 




#
# Bagging
#


resultado_h1n1_bagging <- bagging_JRip(training, test, 500, 1:33,
									  training_labels[,1])


resultado_seasonal_bagging <- bagging_JRip(training, test, 500, 1:33,
										   training_labels[,2])

resultados_bagging <- data.frame(respondent_id = c(26707:53414), 
								 h1n1_vaccine = unname(resultado_h1n1_bagging), 
								 seasonal_vaccine = unname(resultado_seasonal_bagging))

write.csv(resultados_bagging, "../results/JRip_bagging_500.csv", row.names = F)  



# Bagging usando mediana como funcion de agregación
 
resultado_h1n1_bagging <- bagging_JRip(training, test, 500, 1:33,
									   training_labels[,1], funcion_agregacion = "mediana")


resultado_seasonal_bagging <- bagging_JRip(training, test, 500, 1:33,
										   training_labels[,2], funcion_agregacion = "mediana")

resultados_bagging <- data.frame(respondent_id = c(26707:53414), 
								 h1n1_vaccine = unname(resultado_h1n1_bagging), 
								 seasonal_vaccine = unname(resultado_seasonal_bagging))

write.csv(resultados_bagging, "../results/JRip_bagging_500_mediana.csv", row.names = F)  


#
# Bagging usando h1n1 para predecir seasonal
#

training_con_h1n1 <- base::cbind(training, training_labels[,1])

test_con_predicciones_h1n1 <- base::cbind(test, h1n1_vaccine = ifelse(resultado_h1n1_bagging < 0.5, 0, 1))
test_con_predicciones_h1n1$h1n1_vaccine <- as.factor(test_con_predicciones_h1n1$h1n1_vaccine)


resultado_seasonal_bagging_con_h1n1 <- bagging_JRip(training_con_h1n1, test_con_predicciones_h1n1, 500, 1:33,
										   training_labels[,2])


resultados_bagging_con_h1n1 <- data.frame(respondent_id = c(26707:53414), 
								 h1n1_vaccine = unname(resultado_h1n1_bagging), 
								 seasonal_vaccine = unname(resultado_seasonal_bagging_con_h1n1))

write.csv(resultados_bagging_con_h1n1, "../results/JRip_bagging_500_prediciendo_con_h1n1.csv", row.names = F)  


#
# Bagging usando seasonal para predecir h1n1
#

training_con_seasonal <- base::cbind(training, training_labels[,2])

test_con_predicciones_seasonal <- base::cbind(test, seasonal_vaccine = ifelse(resultado_seasonal_bagging < 0.5, 0, 1))
test_con_predicciones_seasonal$seasonal_vaccine <- as.factor(test_con_predicciones_seasonal$seasonal_vaccine)


resultado_h1n1_bagging_con_seasonal <- bagging_JRip(training_con_seasonal, test_con_predicciones_seasonal, 500, 1:33,
													training_labels[,1])


resultados_bagging_con_seasonal <- data.frame(respondent_id = c(26707:53414), 
										  h1n1_vaccine = unname(resultado_h1n1_bagging_con_seasonal), 
										  seasonal_vaccine = unname(resultado_seasonal_bagging))

write.csv(resultados_bagging_con_seasonal, "../results/JRip_bagging_500_prediciendo_con_seasonal.csv", row.names = F)  


#
# Prediccion con menos variables, las más importantes
#

training_final <- training %>% select(opinion_seas_vacc_effective, opinion_seas_risk,
									  hhs_geo_region, age_group, doctor_recc_h1n1,
									  opinion_h1n1_risk, opinion_h1n1_vacc_effective,
									  education, h1n1_concern, doctor_recc_seasonal)

test_final <- test %>% select(opinion_seas_vacc_effective, opinion_seas_risk,
								  hhs_geo_region, age_group, doctor_recc_h1n1,
								  opinion_h1n1_risk, opinion_h1n1_vacc_effective,
								  education, h1n1_concern, doctor_recc_seasonal)

resultado_h1n1_bagging <- bagging_JRip(training_final, test_final, 500, 1:10,
									   training_labels[,1])


resultado_seasonal_bagging <- bagging_JRip(training_final, test_final, 500, 1:10,
										   training_labels[,2])

resultados_bagging <- data.frame(respondent_id = c(26707:53414), 
								 h1n1_vaccine = unname(resultado_h1n1_bagging), 
								 seasonal_vaccine = unname(resultado_seasonal_bagging))

write.csv(resultados_bagging, "../results/JRip_bagging_500_pocos_predictores.csv", row.names = F)  

