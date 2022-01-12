library(readr)

media_csv <- function(vector_nombres) {
	
	resultados_h1n1 <- vector(mode = "numeric", length = 26708)
	resultados_seasonal <- vector(mode = "numeric", length = 26708)
	

	for (nombre in vector_nombres){
		# leemos el fichero
		resultados <- read_csv(nombre)
		
		resultados_h1n1 <- resultados_h1n1 + resultados$h1n1_vaccine
		resultados_seasonal <- resultados_seasonal + resultados$seasonal_vaccine
	}
	
	respuesta_h1n1 <- resultados_h1n1 / length(vector_nombres)
	respuesta_seasonal <- resultados_seasonal / length(vector_nombres)
	
	resultados <- data.frame(respondent_id = c(26707:53414), 
							  h1n1_vaccine = unname(respuesta_h1n1), 
							  seasonal_vaccine = unname(respuesta_seasonal))
	
	resultados
}

resultados <- media_csv(c("../results/Bagging_C45_J48_results.csv", "../results/CART_RPART_results.csv",
						"../results/JRip_bagging_500.csv", "../results/KNN_hamming_seas.csv", "../results/Bayes_Basic_FS_results.csv"))

write.csv(resultados, "../results/media_ensembles_completo.csv", row.names = F)  
