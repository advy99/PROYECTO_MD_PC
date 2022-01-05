library(tidyverse)
library(tidymodels)
library(forcats)
library(embed)
source("./utils.R")


data_features = preprocesamiento_entrenamiento() 

train_seas <- data_features %>% select(-h1n1_vaccine,-respondent_id...1,
                                           -respondent_id...37) %>%
  select(opinion_seas_vacc_effective, 
         opinion_seas_risk, 
         hhs_geo_region, 
         doctor_recc_seasonal, 
         opinion_h1n1_risk,
         education_ord,
         opinion_h1n1_sick_from_vacc,
         h1n1_knowledge,
         opinion_h1n1_vacc_effective, 
         opinion_seas_sick_from_vacc,
         h1n1_concern,
         seasonal_vaccine)


knn_seas_recipe <- recipe(seasonal_vaccine ~ ., data=train_seas) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_impute_bag(all_predictors()) %>%
  step_woe(all_nominal_predictors(),outcome=vars(seasonal_vaccine)) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) 

knn_model <- nearest_neighbor(neighbors = tune()) %>% set_mode("classification")
set.seed(1)
knn_seas_wf <- workflow() %>% add_model(knn_model) %>% add_recipe(knn_seas_recipe)

knn_grid <- grid_regular(
  neighbors(range= c(5,30)),
  levels=5
)

kfolds <- vfold_cv(train_seas,v=5)
knn_woe <- knn_seas_wf %>% tune_grid(
  resamples=kfolds,
  grid = knn_grid
)


knn_woe_res <- knn_woe %>% collect_metrics()  %>% mutate(familia="knn_woe_enc",
                                                                             target="seasonal_vaccine")

read_csv("./models_cv_results.csv") %>% view(
)

read_csv("./models_cv_results.csv") %>% bind_rows(knn_woe_res) %>%
  filter(target=="seasonal_vaccine") %>%
  ggplot(aes(x=neighbors, y=mean, color=familia)) + geom_line() + facet_wrap(vars(.metric))

#H1N1

train_h1n1 <- data_features %>% select(-respondent_id...1, -respondent_id...37) %>%
  select(doctor_recc_h1n1, 
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
         h1n1_vaccine)  %>% mutate(h1n1_vaccine = as.factor(h1n1_vaccine))


knn_h1n1_recipe <- recipe(h1n1_vaccine ~ ., data=train_h1n1) %>% 
  step_string2factor(all_nominal_predictors()) %>%
  step_impute_bag(all_predictors()) %>%
  step_woe(all_nominal_predictors(), outcome=vars(h1n1_vaccine)) %>%
  step_scale(all_numeric_predictors()) %>%
  step_center(all_numeric_predictors())

knn_model <- nearest_neighbor(neighbors = tune()) %>% set_mode("classification")
set.seed(1)
knn_h1n1_wf <- workflow() %>% add_model(knn_model) %>% add_recipe(knn_h1n1_recipe)

kfolds <- vfold_cv(train_h1n1,v=5)
knn_woe_h1n1 <- knn_h1n1_wf %>% tune_grid(
  resamples=kfolds,
  grid = knn_grid
)

knn_woe_h1n1_res <- knn_woe_h1n1 %>% collect_metrics()  %>% mutate(familia="knn_woe_enc",
                                                                             target="h1n1_vaccine")



read_csv("./models_cv_results.csv") %>% bind_rows(knn_woe_res, knn_woe_h1n1_res) %>%
  filter(.metric=="roc_auc") %>%
  ggplot(aes(x=neighbors, y=mean, color=familia)) + geom_line() + facet_wrap(vars(target))


#Sacada de datos par atest


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
  select(doctor_recc_h1n1, 
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

knn_model <- nearest_neighbor(neighbors = 23) %>% set_mode("classification")
knn_h1n1_wf <- workflow() %>% add_model(knn_model) %>% add_recipe(knn_h1n1_recipe)

knn_h1n1_fit <- knn_h1n1_wf %>% fit(data=train_h1n1) 
knn_h1n1_fit

predictions_h1n1 <- predict(knn_h1n1_fit, test_h1n1_features, type="prob")

test_features_seas <- test_features %>% select(-respondent_id) %>%   select(opinion_seas_vacc_effective, 
                                                                            opinion_seas_risk, 
                                                                            hhs_geo_region, 
                                                                            doctor_recc_seasonal, 
                                                                            opinion_h1n1_risk,
                                                                            education_ord,
                                                                            opinion_h1n1_sick_from_vacc,
                                                                            h1n1_knowledge,
                                                                            opinion_h1n1_vacc_effective, 
                                                                            opinion_seas_sick_from_vacc,
                                                                            h1n1_concern)

knn_seas_wf <- workflow() %>% add_model(knn_model) %>% add_recipe(knn_seas_recipe)
knn_seas_fit <- knn_seas_wf %>% fit(data=train_seas) 

predictions_seas <- predict(knn_seas_fit, test_features_seas,type="prob")

resultados = calcular_predicciones(knn_h1n1_fit,knn_seas_fit,test_features_seas, test_h1n1_features)

view(resultados)
write.csv(resultados,"../../results/KNN_woe_encoding.csv",row.names = F)


