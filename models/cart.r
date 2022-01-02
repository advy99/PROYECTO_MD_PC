library(tidymodels)
library(readr)
library(future)

source("funciones.r")

                                        # PREPROCESSING
## leemos los datos de train
x_train <- read_csv("../data/training_set_features_preprocessed.csv")
x_test <- read_csv("../data/test_set_features_preprocessed.csv")
y_train <- read_csv("../data/training_set_labels.csv")

##convertimos a factor ordenado las preguntas que las respuestas tienen cierto orden
x_train <- x_train %>% mutate(across(c(1,2,16:23,26), as.ordered))
x_test <- x_test %>% mutate(across(c(1,2,16:23,26), as.ordered))

##El resto de variables, menos household_adults y household_children las pasamos
##a factor sin orden.
x_train <- x_train %>% mutate(across(-c(1,2,16:23,26:28), as.factor))
x_test <- x_test %>% mutate(across(-c(1,2,16:23,26:28), as.factor))

## le damos niveles de prioridad a la variable educacion
levels(x_train$education) = c("< 12 Years","12 Years","Some College","College Graduate")
levels(x_test$education) = c("< 12 Years","12 Years","Some College","College Graduate")

## le damos niveles de prioridad a los ingresos
levels(x_train$income_poverty) = c("Below Poverty","<= $75,000, Above Poverty",
                                   "> $75,000")
levels(x_test$income_poverty) = c("Below Poverty","<= $75,000, Above Poverty",
                                  "> $75,000")


## eliminamos el ID de las etiquetas de train
y_train = y_train %>% select(-1)

## el juntarlas es opcional, por si queremos hacer pruebas de predecir
## una sola variable
## if (juntar_etiquetas) {
##     ## juntamos las variables a predecir en una sola columna
##     y_train <- tidyr::unite(y_train, Y, c(h1n1_vaccine,seasonal_vaccine), sep="", remove = TRUE)

## }

## cambiamos todas a factor
y_train <- y_train %>% mutate_all(as.factor)

## añadimos las etiquetas en el conjunto de entrenamiento
x_train$seasonal_vaccine <- y_train$seasonal_vaccine
x_train$h1n1_vaccine <- y_train$h1n1_vaccine


## SEASONAL_VACCINE

num_trees <- 100

predictions <- vector(mode='list', length=num_trees)

entrenar_arbol <- function(train, y_train, test){
    aux <- random_sample(x_train %>% select(-behavioral_antiviral_meds,-behavioral_face_mask,-child_under_6_months,-behavioral_wash_hands)
                        ,x_test,0.8,10)

    x_train_forest <- aux[[1]]
        x_train_forest <- select(x_train_forest,-h1n1_vaccine)
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
    predictions[[i]] <- entrenar_arbol(x_train,y_train,x_test)
}

seasonal_predicted <- (Reduce("+", predictions)/num_trees)

##set.seed(100)
##plan(multisession)

##split <- initial_split(x_seasonal)
##train <- training(split)
##cv <- vfold_cv(train)

##param_res <- tune_grid(dt, rec, resamples = cv, grid = 10)
##show_best(param_res, metric = "roc_auc")

##best_tree <- select_best(param_res, metric = "roc_auc")
##final_tree <- 
## wflow_dt %>% 
## finalize_workflow(best_tree)
## seasonal_predictor <- fit(final_tree,x_seasonal)



## H1N1_VACCINE

num_trees <- 100

predictions <- vector(mode='list', length=num_trees)

entrenar_arbol <- function(train, y_train, test){
    aux <- random_sample(x_train %>% select(-behavioral_antiviral_meds,-behavioral_face_mask,-child_under_6_months,-behavioral_wash_hands)
                        ,x_test,0.9,10)

    x_train_forest <- aux[[1]]
    x_train_forest <- select(x_train_forest,-seasonal_vaccine)
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
    predictions[[i]] <- entrenar_arbol(x_train,y_train,x_test)
}

h1n1_predicted <- (Reduce("+", predictions)/num_trees)


## RESULT

summary(seasonal_predicted)
summary(h1n1_predicted)

prediction <- data.frame(26707:53414,h1n1_predicted,seasonal_predicted)
     colnames(prediction) <- c("respondent_id", "h1n1_vaccine", "seasonal_vaccine")
     summary(prediction)

write.csv(prediction,"prediction.csv", row.names = F)
