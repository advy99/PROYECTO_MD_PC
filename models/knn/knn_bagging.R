library(tidyverse)
library(tidymodels)
library(forcats)
library(KernelKnn)
library(doParallel)
library(foreach)

source("./utils.R")
train = preprocesamiento_entrenamiento() 

train_h1n1 = train %>%   select(doctor_recc_h1n1, 
                                hhs_geo_region, 
                                opinion_h1n1_risk, 
                                opinion_h1n1_vacc_effective, 
                                age_group_ord,
                                opinion_seas_risk,
                                opinion_h1n1_sick_from_vacc,
                                education_ord,
                                opinion_seas_sick_from_vacc,
                                h1n1_concern,
                                opinion_seas_vacc_effective,
                                h1n1_vaccine) %>%
  mutate(h1n1_vaccine = as.numeric(h1n1_vaccine))

knn_h1n1_recipe <- recipe(h1n1_vaccine ~ ., data=train_h1n1) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_impute_bag(all_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_scale(all_numeric_predictors())  %>%
  step_center(all_numeric_predictors()) %>% prep()

knn_grid <- grid_regular(
  neighbors(range= c(1,7)),
  trees(range = c(10, 100)),
  levels=4
) %>% rename(knns = trees)
knn_grid

cl<-makeCluster(8)
registerDoParallel(cl)

results_h1n1 <- knn_grid %>% pmap_dfr(function(...){
  current <- tibble(...)
  k <- current$neighbors[[1]]
  num_of_knns<- current$knns[[1]]
  
  fold_results <- map_dfr(1:5, function(id){
    split <- initial_split(train_h1n1)
    #En este caso no hacemos validación cruzada con el preprocesamiento
    train_data <- training(split)
    train_cv <- bake(knn_h1n1_recipe,new_data=train_data)
    test_cv <- bake(knn_h1n1_recipe, new_data=testing(split))
    test_labels <-test_cv$h1n1_vaccine
    train_labels <- train_cv$h1n1_vaccine
    train <- select(train_cv, -h1n1_vaccine)
    test <- select(test_cv, - h1n1_vaccine)
    #Paralelizacion sacada de https://amunategui.github.io/bagging-in-R/
    predictions <- foreach(m=1:num_of_knns, .combine=rbind, .packages = c("KernelKnn","tibble","dplyr","tidyr"),
                           .export = c("k","train","test")) %dopar% {
      sample_rows <- sample(nrow(train_cv), size=nrow(train_cv)*(2/3))

      predictions <- 
        as_tibble(KernelKnn(data=train[sample_rows,],
                            TEST_data=test, 
                            y=train_labels[sample_rows], 
                            k=as.numeric(k), 
                            method = "hamming",
                            Levels = c(1,2)))
      predictions$index <- 1:nrow(predictions)
      predictions <- predictions %>% pivot_longer(c(class_1,class_2), names_to = "class", values_to = "pred") 
    }
    predictions <- as_tibble(predictions) %>% group_by(class,index) %>% summarise(pred = mean(pred)) %>%
      ungroup() %>% pivot_wider(names_from = class, values_from = pred, id_cols=index) %>% select(-index)
    #La libreria obliga a pasar las etiquetas como numerics asi que transformar la clase 1 a la 1 y la 0 a la 2.
    #Por eso a posteriori hacemos distintos cambios
    
    class_pred <- colnames(predictions)[max.col(predictions, ties.method = "first")] 
    class_pred <- recode(class_pred, 
                         "class_1"= 0,
                         "class_2"= 1)
    
    metric_matrix <- tibble(truth=as.factor(recode(test_labels,
                                                   "1"=0,
                                                   "2"=1)),
                            class_true=predictions$class_2,
                            class_false=predictions$class_1,
                            predicted=as.factor(class_pred))
    
    roc_auc_res = roc_auc(metric_matrix,truth=truth,estimate=class_false)
    accuracy_res = accuracy(metric_matrix,truth,predicted)
    
    tibble(
      neighbors=k,
      id = id,
      num_of_knns <- num_of_knns,
      roc_auc= roc_auc_res$.estimate[[1]],
      accuracy= accuracy_res$.estimate[[1]]
    )
  })
  fold_results
})
results_h1n1 %>% view()

train_seas = train %>% select(opinion_seas_vacc_effective, 
                              opinion_seas_risk, 
                              hhs_geo_region, 
                              age_group_ord,
                              doctor_recc_seasonal, 
                              opinion_h1n1_risk, 
                              opinion_h1n1_vacc_effective, 
                              opinion_seas_sick_from_vacc,
                              h1n1_concern,
                              seasonal_vaccine,
                              education_ord,
                              opinion_h1n1_sick_from_vacc) %>%
  mutate(seasonal_vaccine = as.numeric(seasonal_vaccine))

knn_seas_recipe <- recipe(seasonal_vaccine ~ ., data=train_seas) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_impute_bag(all_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_scale(all_numeric_predictors())  %>%
  step_center(all_numeric_predictors()) %>% prep()


results_seas <- knn_grid %>% pmap_dfr(function(...){
  current <- tibble(...)
  k <- current$neighbors[[1]]
  num_of_knns<- current$knns[[1]]
  
  fold_results <- map_dfr(1:5, function(id){
    split <- initial_split(train_seas)
    #En este caso no hacemos validación cruzada con el preprocesamiento
    train_data <- training(split)
    train_cv <- bake(knn_seas_recipe,new_data=train_data)
    test_cv <- bake(knn_seas_recipe, new_data=testing(split))
    test_labels <-test_cv$seasonal_vaccine
    train_labels <- train_cv$seasonal_vaccine
    train <- select(train_cv, -seasonal_vaccine)
    test <- select(test_cv, - seasonal_vaccine)
    #Paralelizacion sacada de https://amunategui.github.io/bagging-in-R/
    predictions <- foreach(m=1:num_of_knns, .combine=rbind, .packages = c("KernelKnn","tibble","dplyr","tidyr"),
                           .export = c("k","train","test")) %dopar% {
                             sample_rows <- sample(nrow(train_cv), size=nrow(train_cv)*(2/3))
                             
                             predictions <- 
                               as_tibble(KernelKnn(data=train[sample_rows,],
                                                   TEST_data=test, 
                                                   y=train_labels[sample_rows], 
                                                   k=as.numeric(k), 
                                                   method = "euclidean",
                                                   Levels = c(1,2)))
                             predictions$index <- 1:nrow(predictions)
                             predictions <- predictions %>% pivot_longer(c(class_1,class_2), names_to = "class", values_to = "pred") 
                           }
    predictions <- as_tibble(predictions) %>% group_by(class,index) %>% summarise(pred = mean(pred)) %>%
      ungroup() %>% pivot_wider(names_from = class, values_from = pred, id_cols=index) %>% select(-index)
    #La libreria obliga a pasar las etiquetas como numerics asi que transformar la clase 1 a la 1 y la 0 a la 2.
    #Por eso a posteriori hacemos distintos cambios
    
    class_pred <- colnames(predictions)[max.col(predictions, ties.method = "first")] 
    class_pred <- recode(class_pred, 
                         "class_1"= 0,
                         "class_2"= 1)
    
    metric_matrix <- tibble(truth=as.factor(recode(test_labels,
                                                   "1"=0,
                                                   "2"=1)),
                            class_true=predictions$class_2,
                            class_false=predictions$class_1,
                            predicted=as.factor(class_pred))
    
    roc_auc_res = roc_auc(metric_matrix,truth=truth,estimate=class_false)
    accuracy_res = accuracy(metric_matrix,truth,predicted)
    
    tibble(
      neighbors=k,
      id = id,
      num_of_knns = num_of_knns,
      roc_auc= roc_auc_res$.estimate[[1]],
      accuracy= accuracy_res$.estimate[[1]]
    )
  })
  fold_results
})

results_h1n1_format <- read_csv("./results_h1n1_bagging.csv") %>% rename(num_of_knns = `num_of_knns <- num_of_knns` ) %>% 
  group_by(num_of_knns,neighbors) %>% summarise(roc_auc = mean(roc_auc),
                                              roc_auc_std_err= sd(roc_auc),
                                              accuracy_std_err = sd(accuracy),
                                              accuracy = mean(accuracy)) %>%
  ungroup() %>% mutate(.config="a",familia="knn_hamming_bagging", target="h1n1_vaccine",
                       .estimator="binary", n=5)  %>%
  pivot_longer(cols=c(roc_auc, accuracy), names_to = ".metric", values_to="mean") %>% 
  select(-roc_auc_std_err) %>% rename(std_err = accuracy_std_err)

results_h1n1_format %>% ggplot(aes(x=neighbors,y = mean, color=as.factor(num_of_knns))) + 
  geom_line() + facet_wrap(vars(.metric))


results_h1n1_format %>% ggplot(aes(x=num_of_knns,y = mean, color=as.factor(neighbors))) + 
  geom_line() + facet_wrap(vars(.metric)) + labs(color="Vecinos", x= "Número de KNNs", y="Media")

results_h1n1_format %>% view()

results_seas_format <- read_csv("./results_seas_bagging.csv") %>%   group_by(num_of_knns,neighbors) %>% summarise(roc_auc = mean(roc_auc),
                                                                                           roc_auc_std_err= sd(roc_auc),
                                                                                           accuracy_std_err = sd(accuracy),
                                                                                           accuracy = mean(accuracy)) %>%
  ungroup() %>% mutate(.config="a",familia="knn_euc_bagging", target="seasonal_vaccine",
                       .estimator="binary", n=5)  %>%
  pivot_longer(cols=c(roc_auc, accuracy), names_to = ".metric", values_to="mean") %>% 
  select(-roc_auc_std_err) %>% rename(std_err = accuracy_std_err)

results_seas_format %>% ggplot(aes(x=num_of_knns,y = mean, color=as.factor(neighbors))) + 
  geom_line() + facet_wrap(vars(.metric)) + labs(color="Vecinos", x= "Número de KNNs", y="Media")


read_csv("./models_cv_results.csv") %>% filter(.metric=="roc_auc", target=="h1n1_vaccine") %>% arrange(-mean) %>% view()
read_csv("./models_cv_results.csv") %>% filter(.metric=="roc_auc", target=="seasonal_vaccine") %>% arrange(-mean) %>% view()


###Resultados para driven data
train_h1n1 = train %>%   select(doctor_recc_h1n1, 
                                hhs_geo_region, 
                                opinion_h1n1_risk, 
                                opinion_h1n1_vacc_effective, 
                                age_group_ord,
                                opinion_seas_risk,
                                opinion_h1n1_sick_from_vacc,
                                education_ord,
                                opinion_seas_sick_from_vacc,
                                h1n1_concern,
                                opinion_seas_vacc_effective,
                                h1n1_vaccine) %>%
  mutate(h1n1_vaccine = as.numeric(h1n1_vaccine))

knn_h1n1_recipe <- recipe(h1n1_vaccine ~ ., data=train_h1n1) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_impute_bag(all_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_scale(all_numeric_predictors())  %>%
  step_center(all_numeric_predictors()) %>% prep()

train_preprocessed_data <- bake(knn_h1n1_recipe,new_data=train_h1n1)
train_labels <- select(train_preprocessed_data,h1n1_vaccine)
train_h1n1_final <- select(train_preprocessed_data, -h1n1_vaccine)



test_features <- read_csv("../../data/test_set_features.csv") %>% mutate(age_group_ord = recode(age_group,
                                                                                                `18 - 34 Years`= 0,
                                                                                                `35 - 44 Years` = 1,
                                                                                                `45 - 54 Years` = 2,
                                                                                                `55 - 64 Years` = 3,
                                                                                                `65+ Years` = 4)) %>%
  mutate(education_ord = recode(education,
                                `< 12 Years` = 0,
                                `12 Years` = 1,
                                `Some College` = 2,
                                `College Graduate` = 3))

test_h1n1_features <- test_features %>% select(-respondent_id) %>% select(doctor_recc_h1n1, 
                                                                          hhs_geo_region, 
                                                                          opinion_h1n1_risk, 
                                                                          opinion_h1n1_vacc_effective, 
                                                                          age_group_ord,
                                                                          opinion_seas_risk,
                                                                          opinion_h1n1_sick_from_vacc,
                                                                          education_ord,
                                                                          opinion_seas_sick_from_vacc,
                                                                          h1n1_concern,
                                                                          opinion_seas_vacc_effective)

test <- bake(knn_h1n1_recipe, new_data=test_h1n1_features)
sample_rows <- sample(nrow(train_h1n1_final), size=nrow(train_h1n1_final)*(2/3))
sample_rows

predictions <- foreach(m=1:70, .combine=rbind, .packages = c("KernelKnn","tibble","dplyr","tidyr")) %dopar% {
                         sample_rows <- sample(nrow(train_h1n1_final), size=nrow(train_h1n1_final)*(2/3))
                         predictions <- 
                           as_tibble(KernelKnn(data=train_h1n1_final[sample_rows,],
                                               TEST_data=test, 
                                               y=train_labels$h1n1_vaccine[sample_rows], 
                                               k=7, 
                                               method = "hamming",
                                               Levels = c(1,2)))
                         predictions$index <- 1:nrow(predictions)
                         predictions <- predictions %>% pivot_longer(c(class_1,class_2), names_to = "class", values_to = "pred") 
                       }
predictions_h1n1_res <- as_tibble(predictions) %>% group_by(class,index) %>% summarise(pred = mean(pred)) %>%
  ungroup() %>% pivot_wider(names_from = class, values_from = pred, id_cols=index) %>% select(-index)




#prediccion seas

train_seas = train %>% select(opinion_seas_vacc_effective, 
                              opinion_seas_risk, 
                              hhs_geo_region, 
                              age_group_ord,
                              doctor_recc_seasonal, 
                              opinion_h1n1_risk, 
                              opinion_h1n1_vacc_effective, 
                              opinion_seas_sick_from_vacc,
                              h1n1_concern,
                              seasonal_vaccine,
                              education_ord,
                              opinion_h1n1_sick_from_vacc) %>%
  mutate(seasonal_vaccine = as.numeric(seasonal_vaccine))

test_seas_features <- test_features %>% select(-respondent_id) %>%  select(opinion_seas_vacc_effective, 
                                                                           opinion_seas_risk, 
                                                                           hhs_geo_region, 
                                                                           age_group_ord,
                                                                           doctor_recc_seasonal, 
                                                                           opinion_h1n1_risk, 
                                                                           opinion_h1n1_vacc_effective, 
                                                                           opinion_seas_sick_from_vacc,
                                                                           h1n1_concern,
                                                                           education_ord,
                                                                           opinion_h1n1_sick_from_vacc)


knn_seas_recipe_prep <- prep(knn_seas_recipe, training=train_seas)
train <- bake(knn_seas_recipe,new_data=train_seas)
test <- bake(knn_seas_recipe, new_data=test_seas_features)

train_labels <- select(train,seasonal_vaccine)
train <- select(train, -seasonal_vaccine)

predictions_seas<- foreach(m=1:70, .combine=rbind, .packages = c("KernelKnn","tibble","dplyr","tidyr")) %dopar% {
                         sample_rows <- sample(nrow(train), size=nrow(train)*(2/3))
                         
                         predictions <- 
                           as_tibble(KernelKnn(data=train[sample_rows,],
                                               TEST_data=test, 
                                               y=train_labels$seasonal_vaccine[sample_rows], 
                                               k=7, 
                                               method = "euclidean",
                                               Levels = c(1,2)))
                         predictions$index <- 1:nrow(predictions)
                         predictions <- predictions %>% pivot_longer(c(class_1,class_2), names_to = "class", values_to = "pred") 
}

predictions_seas
predictions_seas_res <- as_tibble(predictions_seas) %>% group_by(class,index) %>% summarise(pred = mean(pred)) %>%
  ungroup() %>% pivot_wider(names_from = class, values_from = pred, id_cols=index) %>% select(-index)

data.frame(respondent_id=c(26707:53414),
           h1n1_vaccine=predictions_h1n1_res$class_2,
           seasonal_vaccine=predictions_seas_res$class_2) %>% 
  write.csv("../../results/KNN_bagging_70_7.csv",row.names = F)
