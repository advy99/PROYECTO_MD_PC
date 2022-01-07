# Algoritmo KNN

En esta carpeta están los distintos modelos que se han ido probando. La mayoría de modelos se han entrenado usando la librería tidymodels el resto, aquellos que necesitaban distancias distintas a la euclidea, se han entrado usando KernelKNN. A continuación se describen los archivos:

- *knn.R* : KNN básico k=15, el primero de los enviados a DrivenData
- *knn_bagging.R*: bagging de knns usando validación cruzada. Se corresponde con la sexta entrega.
- *knn_denoise.R* : pruebas de usar el KNN con distintos algoritmos de reducción de ruido. La versión que hay es la que tiene el IPF ya que el otro algoritmo me iba demasiado lento en mi ordenador.
- *knn_dim_reduction.R*: pruebas para usar el KNN con todas las variables con PCA. No lo pude ejecutar en mi ordenador.
- *knn_distancia.R*: selección de distancias  y de ks usando validación cruzada. Los modelos finales se corresponden con la 5 entrega.
- *knn_ponderado.R*: KNN con distancias ponderadas según la importancia de las variables.
- *knn_smote_seas.R*: KNN con SMOTE en h1n1 y usando seas como predictor en h1n1. Se corresponde con la 2 y 3 entrega.
- *knn_woe.R*: KNN usando Weight of Evidence para las variable categórica. Se corresponde con la 4 de las entregas
- *step_weighted_dist.R*: archivo que define el paso sigueindo la sintaxis del tidyverse y de recipes.
- *utils.R*: archivo con funciones básicas de preprocesamiento que incluye renombre de variable y lectura desde el archivo.
- *models_cv_results.csv*: archivo con los resultados de los distintos algoritmos probados (no incluye todos, solo aquellos con resultados notables)
- *driven_data_result.csv*: archivo con los resultados de los algoritmos usbidos a driven data. 
