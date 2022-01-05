library(tidyverse)
library(tidymodels)
library(themis)
source("./utils.R")

data_features <- preprocesamiento_entrenamiento()

train_seas <- data_features %>% select(-h1n1_vaccine,-respondent_id...1,
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
         seasonal_vaccine) %>% mutate(seasonal_vaccine = as.factor(seasonal_vaccine))
colnames(train_seasonal)
 
knn_seas_recipe <- recipe(seasonal_vaccine ~ ., data=train_seas) %>%
  step_impute_knn(all_predictors(),neighbors=11) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())
  

knn_model <- nearest_neighbor(neighbors = tune(),
                              weight_func = tune()) %>% set_mode("classification")
set.seed(1)
knn_seas_wf <- workflow() %>% add_model(knn_model) %>% add_recipe(knn_seas_recipe)


knn_grid <- grid_regular(
  neighbors(range= c(7,40)),
  weight_func(values=c("rectangular","cos","rank","triangular")),
  levels=5
)
knn_grid

kfolds <- vfold_cv(train_seas,v=5)
knn_basico_res <- knn_seas_wf %>% tune_grid(
  resamples=kfolds,
  grid = knn_grid
)

seas_res <- knn_basico_res %>% collect_metrics() %>% mutate(familia="knn_seas_impute",
                                                target="seasonal_vaccine")
seas_res
knn_basico_res %>% collect_metrics() %>% ggplot(aes(x=neighbors, y=mean,color=.metric)) + geom_line() +
  facet_wrap(vars(weight_func))


seas_res %>% bind_rows(read_csv("./models_cv_results.csv")) %>% 
  filter(.metric=="roc_auc",
         target=="seasonal_vaccine") %>% ggplot(aes(x=neighbors, y=mean,color=familia)) + geom_line()



 ## KNN para h1n1

train_h1n1 <- data_features %>% select(-respondent_id...1, -respondent_id...37) %>%
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
         h1n1_knowledge,
         household_adults,
         household_children,
         seasonal_vaccine,
         h1n1_vaccine)  %>% mutate(h1n1_vaccine = as.factor(h1n1_vaccine),
                                   seasonal_vaccine = as.numeric(seasonal_vaccine))

knn_h1n1_recipe <- recipe(h1n1_vaccine ~ ., data=train_h1n1) %>%
  step_impute_knn(all_predictors(),neighbors=11) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  step_smote(h1n1_vaccine)

knn_model <- nearest_neighbor(neighbors = tune()) %>% set_mode("classification")
set.seed(1)
knn_h1n1_wf <- workflow() %>% add_model(knn_model) %>% add_recipe(knn_h1n1_recipe)

kfolds <- vfold_cv(train_h1n1,v=5)
knn_grid <- grid_regular(
  neighbors(range= c(7,40)),
  levels=5
)

knn_h1n1_smote_seas <- knn_h1n1_wf %>% tune_grid(
  resamples=kfolds,
  grid = knn_grid
)

results_smote_seas <- knn_h1n1_smote_seas %>% collect_metrics() %>% mutate(target="h1n1_vaccine",
                                                     familia="knn_smote_seas")

models <- read_csv("./models_cv_results.csv") %>% bind_rows(results)
models %>% filter(target=="h1n1_vaccine", .metric=="roc_auc") %>% arrange(-mean)

#### Modelo sin la seas vacc

train_h1n1 <- data_features %>% select(-respondent_id...1, -respondent_id...37) %>%
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
         h1n1_knowledge,
         household_adults,
         household_children,
         h1n1_vaccine)  %>% mutate(h1n1_vaccine = as.factor(h1n1_vaccine),)

knn_h1n1_recipe <- recipe(h1n1_vaccine ~ ., data=train_h1n1) %>%
  step_impute_knn(all_predictors(),neighbors=23) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  step_smote(h1n1_vaccine)

knn_model <- nearest_neighbor(neighbors = tune()) %>% set_mode("classification")
set.seed(1)
knn_h1n1_wf <- workflow() %>% add_model(knn_model) %>% add_recipe(knn_h1n1_recipe)

kfolds <- vfold_cv(train_h1n1,v=5)
knn_grid <- grid_regular(
  neighbors(range= c(7,40)),
  levels=5
)

knn_h1n1_smote <- knn_h1n1_wf %>% tune_grid(
  resamples=kfolds,
  grid = knn_grid
)

results <- knn_h1n1_smote %>% collect_metrics() %>% mutate(target="h1n1_vaccine",
                                                                familia="knn_smote")
results

models <- read_csv("./models_cv_results.csv") %>% bind_rows(results, results_smote_seas)
models %>% filter(target=="h1n1_vaccine", .metric=="roc_auc") %>% arrange(-mean)

# Sacamos a test los modelos h1n1_seas_smote y seas con knn impute

test_features <- read_csv("../../data/test_set_features.csv") %>% mutate(age_group_ord = recode(age_group,
                                                                                                `18 - 34 Years`= 0,
                                                                                                `35 - 44 Years` = 1,
                                                                                                `45 - 54 Years` = 2,
                                                                                                `55 - 64 Years` = 3,
                                                                                                `65+ Years` = 4))
k <- 15

test_features_seas <- test_features %>% select(-respondent_id) %>%
  select(opinion_seas_vacc_effective, 
         opinion_seas_risk, 
         hhs_geo_region, 
         age_group_ord,
         doctor_recc_seasonal, 
         opinion_h1n1_risk, 
         opinion_h1n1_vacc_effective, 
         opinion_seas_sick_from_vacc,
         h1n1_knowledge,
         household_adults,
         household_children,
         h1n1_concern)

knn_seas_recipe <- recipe(seasonal_vaccine ~ ., data=train_seas) %>%
  step_impute_knn(all_predictors(),neighbors=k) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())

knn_model <- nearest_neighbor(neighbors = k) %>% set_mode("classification")
knn_seas_wf <- workflow() %>% add_model(knn_model) %>% add_recipe(knn_seas_recipe)
knn_seas_fit <- knn_seas_wf %>% fit(data=train_seas) 

predictions_seas <- predict(knn_seas_fit, test_features_seas,type="prob")
predictions_seas_class <- predict(knn_seas_fit, test_features_seas)



class_seas <- predictions_seas_class %>% rename(seasonal_vaccine = .pred_class) %>% 
  mutate(seasonal_vaccine = as.numeric(seasonal_vaccine))

test_h1n1_features <- test_features %>% bind_cols(class_seas) %>% select(-respondent_id) %>%
  select(opinion_seas_vacc_effective, 
         opinion_seas_risk, 
         hhs_geo_region, 
         seasonal_vaccine,
         age_group,
         education,
         doctor_recc_h1n1, 
         opinion_h1n1_risk, 
         opinion_h1n1_vacc_effective, 
         opinion_seas_sick_from_vacc,
         h1n1_knowledge,
         household_adults,
         household_children,
         h1n1_concern,) %>% mutate(seasonal_vaccine = as.numeric(seasonal_vaccine))

knn_h1n1_recipe <- recipe(h1n1_vaccine ~ ., data=train_h1n1) %>%
  step_impute_knn(all_predictors(),neighbors=23) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>%
  step_smote(h1n1_vaccine)

knn_h1n1_wf <- workflow() %>% add_model(knn_model) %>% add_recipe(knn_h1n1_recipe)

knn_h1n1_fit <- knn_h1n1_wf %>% fit(data=train_h1n1) 
knn_h1n1_fit

predictions_h1n1 <- predict(knn_h1n1_fit, test_h1n1_features, type="prob")

colnames(train_seasonal)

resultados = calcular_predicciones(knn_h1n1_fit,knn_seas_fit,test_features_seas, test_h1n1_features)
view(resultados)
res_2 <- read_csv("../../results/KNN_smote_semisupervised.csv")
sum(res_2 == resultados)
sum(res_2 != resultados)

write.csv(resultados,"../../results/KNN_smote_semisupervised_k=15.csv",row.names = F)

