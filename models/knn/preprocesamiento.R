feat_file <- "../../data/training_set_features.csv"
labels_file <- "../../data/training_set_labels.csv"

preprocesamiento_entrenamiento <- function(training_features_file=labels_file, training_labels_file=feat_file){
  data_features <- read_csv(training_labels_file)
  data_labels <- read_csv(training_features_file)
  
  data_features %>% bind_cols(data_labels) %>% preprocesamiento_general() %>% 
    mutate(seasonal_vaccine = as.factor(seasonal_vaccine),
          h1n1_vaccine = as.factor(h1n1_vaccine))
}

preprocesamiento_general <- function(.data){
  mutate(.data, age_group_ord = recode(age_group,
                                        `18 - 34 Years`= 0,
                                        `35 - 44 Years` = 1,
                                        `45 - 54 Years` = 2,
                                        `55 - 64 Years` = 3,
                                        `65+ Years` = 4))
}


calcular_predicciones <- function(h1n1_fit, seas_fit, test_tibble_seas, test_tibble_h1n1){
  predictions_seas <- predict(seas_fit, test_tibble_seas,type="prob")
  predictions_h1n1 <- predict(h1n1_fit, test_tibble_h1n1,type="prob")
  
  data.frame(respondent_id=c(26707:53414),
             h1n1_vaccine=predictions_h1n1$.pred_1,
             seasonal_vaccine=predictions_seas$.pred_1)
}


