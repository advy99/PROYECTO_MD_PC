library(randomForest)
library(tidyverse)
library(tidymodels)
library(rlang)
library(modeldata)

source("./utils.R")

data_features <- preprocesamiento_entrenamiento()
train_seasonal <- data_features %>% select(-h1n1_vaccine,-respondent_id...1,
                                       -respondent_id...37) %>%
  select(opinion_seas_vacc_effective, 
         opinion_seas_risk, 
         hhs_geo_region, 
         age_group_ord,
         doctor_recc_seasonal, 
         opinion_h1n1_risk, 
         opinion_h1n1_vacc_effective, 
         opinion_seas_sick_from_vacc,
         h1n1_concern,
         household_children,
         household_adults,
         education_ord,
         seasonal_vaccine)


knn_seasonal_recipe <- recipe(seasonal_vaccine ~ ., data=train_seasonal) %>%
  step_impute_bag(all_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  step_weighted_dist(seasonal_vaccine)


knn_model <- nearest_neighbor(neighbors = tune()) %>% set_mode("classification")
set.seed(1)

knn_seas_wf <- workflow() %>% add_model(knn_model) %>% add_recipe(knn_seasonal_recipe)


knn_grid <- grid_regular(
  neighbors(range= c(5,30)),
  levels=5
)
knn_grid
kfolds <- vfold_cv(train_seasonal,v=5)
knn_weighted_res <- knn_seas_wf %>% tune_grid(
  resamples=kfolds,
  grid = knn_grid
)
seas_weighted <- knn_weighted_res %>% collect_metrics() %>% mutate(familia="knn_weighted",
                                                            target="seasonal_vaccine")

knn_basico_res %>% collect_metrics() %>% ggplot(aes(x=neighbors, y=mean,color=.metric)) + geom_line()

read_csv("./models_cv_results.csv") %>% filter(target=="seasonal_vaccine", neighbors == 23)


## KNN para h1n1

train_h1n1 <- data_features %>% select(-respondent_id...1, -respondent_id...37) %>%
  select(opinion_seas_vacc_effective, 
         opinion_seas_risk, 
         hhs_geo_region, 
         age_group_ord,
         education_ord,
         doctor_recc_h1n1, 
         opinion_h1n1_risk, 
         opinion_h1n1_vacc_effective, 
         opinion_seas_sick_from_vacc,
         h1n1_concern,
         h1n1_knowledge,
         household_adults,
         household_children,
         h1n1_vaccine)  %>% mutate(h1n1_vaccine = as.factor(h1n1_vaccine))


knn_h1n1_recipe <- recipe(h1n1_vaccine ~ ., data=train_h1n1) %>% 
  step_impute_bag(all_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_weighted_dist(h1n1_vaccine)

knn_model <- nearest_neighbor(neighbors = tune()) %>% set_mode("classification")
set.seed(1)
knn_h1n1_wf <- workflow() %>% add_model(knn_model) %>% add_recipe(knn_h1n1_recipe)

kfolds <- vfold_cv(train_h1n1,v=5)
knn_basico_h1n1 <- knn_h1n1_wf %>% tune_grid(
  resamples=kfolds,
  grid = knn_grid
)

knn_weighted_h1n1_res_2 <- knn_basico_h1n1 %>% collect_metrics()  %>% mutate(familia="knn_weighted2",
                                                  target="h1n1_vaccine")

knn_weighted_h1n1_res %>% filter(target=="h1n1_vaccine", neighbors == 23)

read_csv("./models_cv_results.csv") %>%
  filter(.metric=="roc_auc") %>%
  ggplot(aes(x=neighbors, y=mean, color=familia)) + geom_line() + facet_wrap(vars(target))



#No existe ninguna mejora sustancia utilizando los esquemas de pesos
#Si que existe mejora cuando se mete la seasonal_vaccine en la h1n1.

#RESULTADDOS

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

test_h1n1_features <- test_features %>% select(-respondent_id) %>%
  select(opinion_seas_vacc_effective, 
         opinion_seas_risk, 
         hhs_geo_region, 
         age_group,
         education_ord,
         doctor_recc_h1n1, 
         opinion_h1n1_risk, 
         opinion_h1n1_vacc_effective, 
         opinion_seas_sick_from_vacc,
         h1n1_knowledge,
         doctor_recc_h1n1,
         h1n1_concern,)


knn_h1n1_recipe <- recipe(h1n1_vaccine ~ ., data=train_h1n1) %>% 
  step_impute_bag(all_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_weighted_dist(h1n1_vaccine)

knn_model <- nearest_neighbor(neighbors = 23) %>% set_mode("classification")
knn_h1n1_wf <- workflow() %>% add_model(knn_model) %>% add_recipe(knn_h1n1_recipe)

knn_h1n1_fit <- knn_h1n1_wf %>% fit(data=train_h1n1) 
knn_h1n1_fit
predictions_h1n1 <- predict(knn_h1n1_fit, test_features_seas,type="prob")

train_seasonal <- data_features %>% select(-h1n1_vaccine,-respondent_id...1,
                                           -respondent_id...37) %>%
  select(opinion_seas_vacc_effective, 
         opinion_seas_risk, 
         hhs_geo_region, 
         age_group_ord,
         doctor_recc_seasonal, 
         opinion_h1n1_risk, 
         opinion_h1n1_vacc_effective, 
         opinion_seas_sick_from_vacc,
         h1n1_concern,
         household_children,
         household_adults,
         education_ord,
         seasonal_vaccine)

test_features_seas <- test_features %>% select(-respondent_id) %>%
  select(opinion_seas_vacc_effective, 
         opinion_seas_risk, 
         hhs_geo_region, 
         age_group_ord,
         education_ord,
         doctor_recc_seasonal, 
         opinion_h1n1_risk,
         household_children,
         household_adults,
         opinion_h1n1_vacc_effective, 
         opinion_seas_sick_from_vacc,
         h1n1_concern)  

knn_seasonal_recipe <- recipe(seasonal_vaccine ~ ., data=train_seasonal) %>%
  step_impute_bag(all_predictors()) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  step_weighted_dist(seasonal_vaccine)

knn_seas_wf <- workflow() %>% add_model(knn_model) %>% add_recipe(knn_seasonal_recipe)
knn_seas_fit <- knn_seas_wf %>% fit(data=train_seasonal) 

predictions_seas <- predict(knn_seas_fit, test_features_seas,type="prob")

resultados = calcular_predicciones(knn_h1n1_fit,knn_seas_fit,test_features_seas, test_h1n1_features)

view(resultados)
write.csv(resultados,"../../results/KNN_weighted.csv",row.names = F)
