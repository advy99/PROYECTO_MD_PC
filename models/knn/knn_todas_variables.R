library(tidyverse)
library(tidymodels)
library(forcats)
source("./utils.R")
source("./knn_custom_model.R")


train = preprocesamiento_entrenamiento() 
colnames(train) 



str(train)

train = train %>% 
  mutate(across(is.factor,~ fct_explicit_na(.x))) %>%
  mutate(health_insurance = replace_na(health_insurance, -1))

train %>% ggplot(aes(x=health_insurance, fill=h1n1_vaccine)) + geom_bar()
train %>% count(education)

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
                              education,
                              opinion_h1n1_sick_from_vacc,
                              household_adults,
                              health_insurance,
                              household_children) %>%
  mutate(seasonal_vaccine = as.numeric(seasonal_vaccine))

str(train_seas)

data(okc)
colnames(okc)
knn_seas_recipe <- recipe(seasonal_vaccine ~ ., data=train_seas) %>%
  step_sample(size=0.1) %>%
  step_impute_knn(all_predictors(),neighbors=11) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_center(all_numeric_predictors()) %>%
  step_scale(all_numeric_predictors())

knn_grid <- grid_regular(
  neighbors(range= c(5,30)),
  dials::weight_func(values=c("euclidean","hamming","cosine","jaccard","pearson")),
  levels=1
) %>% rename(dist_func=weight_func)
knn_grid

kfolds <- vfold_cv(train_seas,v=5)

results = tibble()
apply(knn_grid,1, function(row){
  k = row["neighbors"]
  dist_func = row["dist_func"]
  fold = 0
  
  for (split in kfolds$splits){
    train_data <- training(split)
    knn_seas_recipe <- prep(knn_seas_recipe, training=train_data)
    train <- bake(knn_seas_recipe,new_data=train_data)
    test <- bake(knn_seas_recipe, new_data=testing(split))

    test_labels <- test$seasonal_vaccine
    train_labels <- train$seasonal_vaccine
    train <- select(train, - seasonal_vaccine)
    test <- select(test, - seasonal_vaccine)
    predictions <- my_knn(train,train_labels,test,k=k,metric=dist_func)
  
    fold <- fold +1
    metric_matrix <- tibble(truth=test_labels,class_true=predictions$pred_1,class_false=predictions$pred_0,
           predicted=predictions$class)
    results <-results %>% add_row(
      ksplit=fold,
      dist_func=dist_func,
      k=k,
      roc_auc=roc_auc(metric_matrix, truth,class_true),
      accuracy=accuracy(metric_matrix,truth,predicted)
    )
  }
})

write_csv(results,"./prueba.csv")




