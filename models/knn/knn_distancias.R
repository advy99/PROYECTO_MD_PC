library(tidyverse)
library(tidymodels)
library(forcats)
library(embed)
library(KernelKnn)

source("./utils.R")
source("./knn_custom_model.R")

train_feat = preprocesamiento_entrenamiento() 

train %>% ggplot(aes(x=health_insurance, fill=h1n1_vaccine)) + geom_bar()
train %>% count(education)

train_seas = train_feat %>% select(opinion_seas_vacc_effective, 
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
  step_center(all_numeric_predictors())


knn_grid <- grid_regular(
  neighbors(range= c(5,30)),
  weight_func(values=c("chebyshev","euclidean","hamming","pearson_correlation")),
  levels=5
)
knn_grid

kfolds <- vfold_cv(train_seas,v=5)
kfolds


results <- knn_grid %>% pmap_dfr(function(...){
 
  current <- tibble(...)
  k <- current$neighbors[[1]]
  metric <- current$weight_func[[1]]
  
  fold_results <- map_dfr(1:5, function(id){
    split <- initial_split(train_seas)
  
    train_data <- training(split)
    knn_seas_recipe_prep <- prep(knn_seas_recipe, training=train_data)
    train_cv <- bake(knn_seas_recipe_prep,new_data=train_data)
    test_cv <- bake(knn_seas_recipe_prep, new_data=testing(split))
    
    test_labels <-test_cv$seasonal_vaccine
    train_labels <- train_cv$seasonal_vaccine
    train <- select(train_cv, -seasonal_vaccine)
    test <- select(test_cv, - seasonal_vaccine)
    #Meter modelo con Reticulate
    
    predictions <- as_tibble(KernelKnn(data=train,TEST_data=test, y=train_labels, k=k, threads = 7,
                                       method = metric,
                             Levels = c(1,2)))

    class_pred <- colnames(predictions)[max.col(predictions, ties.method = "first")] 
    class_pred <- recode(class_pred, 
                         "class_1"= 0,
                         "class_2" = 1)
    
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
      distancia = metric,
      roc_auc= roc_auc_res$.estimate[[1]],
      accuracy= accuracy_res$.estimate[[1]]
    )
  })
  fold_results
})
results %>% write_csv("./distancias.csv")

format_results <- read_csv("./distancias.csv") %>% group_by(distancia,neighbors) %>% summarise(roc_auc = mean(roc_auc),
                                                                             roc_auc_std_err= sd(roc_auc),
                                                                             accuracy_std_err = sd(accuracy),
                                                                   accuracy = mean(accuracy)) %>%
  ungroup() %>% mutate(.config="a",familia=as.character(str_glue("knn_{distancia}")), target="seasonal_vaccine",
                       .estimator="binary", n=5)  %>%
  pivot_longer(cols=c(roc_auc, accuracy), names_to = ".metric", values_to="mean") %>% 
  select(-distancia, -roc_auc_std_err) %>% rename(std_err = accuracy_std_err)

format_results <- read_csv("./models_cv_results.csv") 
format_results %>% ggplot(aes(x=neighbors,y=mean,color=familia)) + geom_line() + facet_wrap(vars(.metric,target)) 
format_results %>% bind_rows(read_csv("./models_cv_results.csv")) %>%
  write_csv("./models_cv_results.csv")

#Vamos con resultados con H1N1

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
  step_center(all_numeric_predictors())


knn_grid <- grid_regular(
  neighbors(range= c(5,30)),
  weight_func(values=c("chebyshev","euclidean","hamming","pearson_correlation")),
  levels=5
)
knn_grid


results_h1n1 <- knn_grid %>% pmap_dfr(function(...){
  current <- tibble(...)
  k <- current$neighbors[[1]]
  metric <- current$weight_func[[1]]
  
  fold_results <- map_dfr(1:5, function(id){
    split <- initial_split(train_h1n1)
    
    train_data <- training(split)
    knn_h1n1_recipe_prep <- prep(knn_h1n1_recipe, training=train_data)
    train_cv <- bake(knn_h1n1_recipe_prep,new_data=train_data)
    test_cv <- bake(knn_h1n1_recipe_prep, new_data=testing(split))
    
    test_labels <-test_cv$h1n1_vaccine
    train_labels <- train_cv$h1n1_vaccine
    train <- select(train_cv, -h1n1_vaccine)
    test <- select(test_cv, - h1n1_vaccine)
    
    #La libreria obliga a pasar las etiquetas como numerics asi que transformar la clase 1 a la 1 y la 0 a la 2.
    #Por eso a posteriori hacemos distintos cambios
    predictions <- as_tibble(KernelKnn(data=train,TEST_data=test, y=train_labels, k=k, threads = 7,
                                       method = metric,
                                       Levels = c(1,2)))
    
    class_pred <- colnames(predictions)[max.col(predictions, ties.method = "first")] 
    class_pred <- recode(class_pred, 
                         "class_1"= 0,
                         "class_2" = 1)
    
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
      distancia = metric,
      roc_auc= roc_auc_res$.estimate[[1]],
      accuracy= accuracy_res$.estimate[[1]]
    )
  })
  fold_results
})

format_results_h1 <- read_csv("./distancias_h1n1.csv") %>% group_by(distancia,neighbors) %>% summarise(roc_auc = mean(roc_auc),
                                                                                               roc_auc_std_err= sd(roc_auc),
                                                                                               accuracy_std_err = sd(accuracy),
                                                                                               accuracy = mean(accuracy)) %>%
  ungroup() %>% mutate(.config="a",familia=as.character(str_glue("knn_{distancia}")), target="h1n1_vaccine",
                       .estimator="binary", n=5)  %>%
  pivot_longer(cols=c(roc_auc, accuracy), names_to = ".metric", values_to="mean") %>% 
  select(-distancia, -roc_auc_std_err) %>% rename(std_err = accuracy_std_err)


read_csv("./models_cv_results.csv") %>% 
  filter(familia %in% c("knn_euclidean","knn_hamming","knn_chebyshev","knn_pearson_correlation"),
         .metric=="roc_auc") %>%
  ggplot(aes(x=neighbors,y=mean,color=familia)) + geom_line() + facet_wrap(vars(target)) + 
  labs(x="Vecinos", y="Media de ROC-AUC")


### Modelo con hamming en seasonal_vaccine con k=23 y distancia euclidea en h1n1 con k=23

k <- 23

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

knn_h1n1_recipe_prep <- prep(knn_h1n1_recipe, training=train_h1n1)
train <- bake(knn_h1n1_recipe_prep,new_data=train_h1n1)
test <- bake(knn_h1n1_recipe_prep, new_data=test_h1n1_features)
train_labels <- train$h1n1_vaccine
train <- select(train, -h1n1_vaccine)


predictions_h1n1 <- as_tibble(KernelKnn(data=train,TEST_data=test, y=train_labels, k=k, threads = 7,
                                   method = "euclidean",
                                   Levels = c(1,2)))

predictions_h1n1  %>% rename(class_false=class_2,class_true=class_1)

#Prediccions seas

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
train <- bake(knn_seas_recipe_prep,new_data=train_seas)
test <- bake(knn_seas_recipe_prep, new_data=test_seas_features)

train_labels <- train$seasonal_vaccine
train <- select(train, -seasonal_vaccine)
#Meter modelo con Reticulate

predictions_seas <- as_tibble(KernelKnn(data=train,TEST_data=test, y=train_labels, k=k, threads = 7,
                                   method = "hamming",
                                   Levels = c(1,2)))

data.frame(respondent_id=c(26707:53414),
           h1n1_vaccine=predictions_h1n1$class_2,
           seasonal_vaccine=predictions_seas$class_2) %>% 
  write.csv("../../results/KNN_hamming_seas.csv",row.names = F)





