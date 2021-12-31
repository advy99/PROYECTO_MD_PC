library(tidyverse)
library(tidymodels)
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
         seasonal_vaccine)
colnames(train_seasonal)

knn_seasonal_recipe <- recipe(seasonal_vaccine ~ ., data=train_seasonal) %>% 
  step_string2factor(hhs_geo_region) %>%
  step_bin2factor(doctor_recc_seasonal) %>% 
  step_impute_bag(all_predictors()) %>% 
  step_scale(all_numeric_predictors())
  

knn_model <- nearest_neighbor(neighbors = tune()) %>% set_mode("classification")
set.seed(1)
knn_seas_wf <- workflow() %>% add_model(knn_model) %>% add_recipe(knn_seasonal_recipe)


knn_grid <- grid_regular(
  neighbors(range= c(5,30)),
  levels=5
)
knn_grid

kfolds <- vfold_cv(train_seasonal,v=5)
knn_basico_res <- knn_seas_wf %>% tune_grid(
  resamples=kfolds,
  grid = knn_grid
)

seas_res <- knn_basico_res %>% collect_metrics() %>% mutate(familia="knn_base",
                                                target="seasonal_vaccine")

knn_basico_res %>% collect_metrics() %>% ggplot(aes(x=neighbors, y=mean,color=.metric)) + geom_line()


 ## KNN para h1n1

train_h1n1 <- data_features %>% select(-seasonal_vaccine,-respondent_id...1,
                                                                      -respondent_id...37) %>%
  select(opinion_seas_vacc_effective, 
         opinion_seas_risk, 
         hhs_geo_region, 
         age_group,
         education,
         doctor_recc_h1n1, 
         opinion_h1n1_risk, 
         opinion_h1n1_vacc_effective, 
         opinion_seas_sick_from_vacc,
         h1n1_concern,
         h1n1_vaccine)
colnames(train_seasonal)

knn_h1n1_recipe <- recipe(h1n1_vaccine ~ ., data=train_h1n1) %>% 
  step_string2factor(hhs_geo_region, age_group, education) %>%
  step_bin2factor(doctor_recc_h1n1) %>% 
  step_impute_bag(all_predictors()) %>% 
  step_scale(all_numeric_predictors())

knn_model <- nearest_neighbor(neighbors = tune()) %>% set_mode("classification")
set.seed(1)
knn_h1n1_wf <- workflow() %>% add_model(knn_model) %>% add_recipe(knn_h1n1_recipe)

kfolds <- vfold_cv(train_h1n1,v=5)
knn_basico_h1n1 <- knn_h1n1_wf %>% tune_grid(
  resamples=kfolds,
  grid = knn_grid
)

knn_basico_h1n1 %>% collect_metrics()  %>% mutate(familia="knn_base",
                                              target="h1n1_vaccine") %>% bind_rows(seas_res) %>%
  write_csv("./models_cv_results.csv")



test_features <- read_csv("../../data/test_set_features.csv") %>% mutate(age_group_ord = recode(age_group,
                                                                                                `18 - 34 Years`= 0,
                                                                                                `35 - 44 Years` = 1,
                                                                                                `45 - 54 Years` = 2,
                                                                                                `55 - 64 Years` = 3,
                                                                                                `65+ Years` = 4))

test_h1n1_features <- test_features %>% select(-respondent_id) %>%
  select(opinion_seas_vacc_effective, 
         opinion_seas_risk, 
         hhs_geo_region, 
         age_group,
         education,
         doctor_recc_h1n1, 
         opinion_h1n1_risk, 
         opinion_h1n1_vacc_effective, 
         opinion_seas_sick_from_vacc,
         h1n1_concern,)

knn_model <- nearest_neighbor(neighbors = 15) %>% set_mode("classification")
knn_h1n1_wf <- workflow() %>% add_model(knn_model) %>% add_recipe(knn_h1n1_recipe)

knn_h1n1_fit <- knn_h1n1_wf %>% fit(data=train_h1n1) 
knn_h1n1_fit

predictions_h1n1 <- predict(knn_h1n1_fit, test_h1n1_features, type="prob")

test_features_seas <- test_features %>% select(-respondent_id) %>%
  select(opinion_seas_vacc_effective, 
         opinion_seas_risk, 
         hhs_geo_region, 
         age_group_ord,
         doctor_recc_seasonal, 
         opinion_h1n1_risk, 
         opinion_h1n1_vacc_effective, 
         opinion_seas_sick_from_vacc,
         h1n1_concern)  
  
knn_seas_wf <- workflow() %>% add_model(knn_model) %>% add_recipe(knn_seasonal_recipe)
knn_seas_fit <- knn_seas_wf %>% fit(data=train_seasonal) 

predictions_seas <- predict(knn_seas_fit, test_features_seas,type="prob")

resultados = calcular_predicciones(knn_h1n1_fit,knn_seas_fit,test_features_seas, test_h1n1_features)

view(resultados)
write.csv(resultados,"../../results/KNN_baseline.csv",row.names = F)

