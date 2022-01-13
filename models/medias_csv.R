library(readr)

# media de los CSV, es posible pasar unas ponderaciones
# si no se pasan ponderaciones,todas tienen el peso de 1 y se hace la media 
# aritmética
media_csv <- function(vector_nombres, ponderaciones = rep(1, times = length(vector_nombres))) {
	
	resultados_h1n1 <- vector(mode = "numeric", length = 26708)
	resultados_seasonal <- vector(mode = "numeric", length = 26708)
	
	num_ponderacion <- 1
	for (nombre in vector_nombres){
		# leemos el fichero
		resultados <- read_csv(nombre)
		
		resultados_h1n1 <- resultados_h1n1 + (resultados$h1n1_vaccine * ponderaciones[num_ponderacion])
		resultados_seasonal <- resultados_seasonal + (resultados$seasonal_vaccine * ponderaciones[num_ponderacion])
		num_ponderacion <- num_ponderacion + 1
	}
	
	respuesta_h1n1 <- resultados_h1n1 / sum(ponderaciones)
	respuesta_seasonal <- resultados_seasonal / sum(ponderaciones)
	
	resultados <- data.frame(respondent_id = c(26707:53414), 
							  h1n1_vaccine = unname(respuesta_h1n1), 
							  seasonal_vaccine = unname(respuesta_seasonal))
	
	resultados
}

resultados <- media_csv(c("../results/Bagging_C45_J48_results.csv", "../results/CART_RPART_results.csv",
						"../results/JRip_bagging_500.csv", "../results/KNN_hamming_seas.csv", "../results/Bayes_Basic_FS_results.csv"),
						c(1, 1.3, 0.8, 0.7, 1.2))

write.csv(resultados, "../results/media_ponderada_ensembles_completo.csv", row.names = F)  

