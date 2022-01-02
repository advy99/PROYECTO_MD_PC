library(tidymodels)
library(tidyverse)
library(readr)
library(future)

source("funciones.r")

datos = leer_datos("../data/training_set_features_preprocessed.csv", 
                   "../data/training_set_labels.csv", 
                   "../data/test_set_features_preprocessed.csv", juntar_etiquetas = F)

x_train = datos[[1]]
y_train = datos[[2]]
x_test = datos[[3]]

x_train$seasonal_vaccine = y_train$seasonal_vaccine
x_train$h1n1_vaccine = y_train$h1n1_vaccine

## SEASONAL_VACCINE

num_trees <- 100

predictions <- vector(mode='list', length=num_trees)

entrenar_arbol <- function(train, test){
    aux <- random_sample(x_train
                        ,x_test,0.8,10,1:33,"seasonal_vaccine")

    x_train_forest <- aux[[1]]
    x_test_forest <- aux[[2]]

    rec <- recipe(seasonal_vaccine ~ .,x_train_forest)

    dt <- decision_tree(cost_complexity = 0.0000005, tree_depth = 10, min_n = 35) %>% 
            set_engine("rpart") %>% 
            set_mode("classification")

        wflow_dt <- workflow() %>% 
            add_recipe(rec) %>%
            add_model(dt)
        
    seasonal_predictor <- fit(wflow_dt,x_train_forest)

    return(predict(seasonal_predictor,x_test_forest, type = "prob")$.pred_1)
}

for (i in 1:num_trees){
    predictions[[i]] <- entrenar_arbol(x_train,x_test)
}

seasonal_predicted <- (Reduce("+", predictions)/num_trees)

## H1N1_VACCINE

num_trees <- 100

predictions <- vector(mode='list', length=num_trees)

entrenar_arbol <- function(train, test){
    aux <- random_sample(x_train,x_test,0.9,10,1:33,"h1n1_vaccine")

    x_train_forest <- aux[[1]]
    x_test_forest <- aux[[2]]

    rec <- recipe(h1n1_vaccine ~ .,x_train_forest)

    dt <- decision_tree(cost_complexity = 0.0000005, tree_depth = 10, min_n = 35) %>% 
        set_engine("rpart") %>% 
        set_mode("classification")

    wflow_dt <- workflow() %>% 
        add_recipe(rec) %>%
        add_model(dt)
    
    predictor <- fit(wflow_dt,x_train_forest)

    return(predict(predictor,x_test_forest, type = "prob")$.pred_1)
}

for (i in 1:num_trees){
    predictions[[i]] <- entrenar_arbol(x_train,x_test)
}

h1n1_predicted <- (Reduce("+", predictions)/num_trees)


## RESULT

summary(seasonal_predicted)
summary(h1n1_predicted)

prediction <- data.frame(26707:53414,h1n1_predicted,seasonal_predicted)
     colnames(prediction) <- c("respondent_id", "h1n1_vaccine", "seasonal_vaccine")
     summary(prediction)

write.csv(prediction,"prediction.csv", row.names = F)
