library(tidyverse)
library(tidymodels)
library(forcats)
library(embed)
source("./utils.R")

data_features = preprocesamiento_entrenamiento() 

train_seas <- data_features %>% select(-h1n1_vaccine,-respondent_id...1,
                                       -respondent_id...37) 


knn_seas_recipe <- recipe(seasonal_vaccine ~ ., data=train_seas) %>%
  step_string2factor(all_nominal_predictors()) %>%
  step_impute_bag(all_predictors()) %>%
  step_woe(all_nominal_predictors(),outcome=vars(seasonal_vaccine)) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors()) %>% 
  step_pca(all_predictors(),num_comp = tune())

knn_model <- nearest_neighbor(neighbors = tune()) %>% set_mode("classification")
set.seed(1)
knn_seas_wf <- workflow() %>% add_model(knn_model) %>% add_recipe(knn_seas_recipe)

knn_grid <- grid_regular(
  neighbors(range= c(5,30)),
  num_comp(range=c(2,11)),
  levels=5
)
knn_grid

kfolds <- vfold_cv(train_seas,v=5)
knn_pca <- knn_seas_wf %>% tune_grid(
  resamples=kfolds,
  grid = knn_grid
)


knn_woe_res <- knn_woe %>% collect_metrics()  %>% mutate(familia="knn_woe_enc",
                                                         target="seasonal_vaccine")
