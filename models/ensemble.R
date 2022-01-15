library(e1071)
library(caret)
library(pROC)
library(FSelectorRcpp)
source("funciones.r")

naive_bayes<-read.csv('../results/layered_bayes_FS.csv')

cart<-read.csv('../results/CART_RPART_results.csv')

C45_J48_bagging<-read.csv('../results/Bagging_C45_J48_results_1000.csv')

JRIP_season_pred <- read.csv('../results/JRip_bagging_500_prediciendo_con_seasonal.csv')

knn_hamming_seas <- read.csv('../results/KNN_hamming_seas.csv')

datos_H1N1<-data.frame(naive_bayes_H1N1<-naive_bayes$h1n1_vaccine,
                  cart_H1N1<-cart$h1n1_vaccine,
                  C45J48_H1N1<-C45_J48_bagging$h1n1_vaccine,
                  JRIP_H1N1<-JRIP_season_pred$h1n1_vaccine,
                  KNN_H1N1<-knn_hamming_seas$h1n1_vaccine
                  )

datos_seas<-data.frame(naive_bayes_H1N1<-naive_bayes$seasonal_vaccine,
                       cart_H1N1<-cart$seasonal_vaccine,
                       C45J48_H1N1<-C45_J48_bagging$seasonal_vaccine,
                       JRIP_H1N1<-JRIP_season_pred$seasonal_vaccine,
                       KNN_H1N1<-knn_hamming_seas$seasonal_vaccine
)


scores=c(naive=0.8219,cart=0.8301,C45=0.8165,JRIP=0.8165,knn=0.8055)
#Haciendo la media
ensemble_mean_h1n1<-apply(datos_H1N1,1,mean)
ensemble_mean_seas<-apply(datos_seas,1,mean)
results_mean = data.frame(respondent_id = c(26707:53414), h1n1_vaccine = 
                          unname(ensemble_mean_h1n1), seasonal_vaccine = 
                          unname(ensemble_mean_seas))

write.csv(results_mean,"../results/ensemble_mean.csv",row.names = F)  

# Media ponderada
scores=c(naive=0.8219,cart=0.8301,C45=0.8165,JRIP=0.8165,knn=0.8055)
scores=scores-mu
sum_scores=sum(scores)
weights=scores/sum(scores)

ensemble_weighted_h1n1<-apply(datos_H1N1,1,function(x) sum(x*weights))
ensemble_weighted_seas<-apply(datos_seas,1,function(x) sum(x*weights))
results_weighted_mean = data.frame(respondent_id = c(26707:53414), h1n1_vaccine = 
                                unname(ensemble_weighted_h1n1), seasonal_vaccine = 
                                unname(ensemble_weighted_seas))
## Guardo los resultados
write.csv(results_weighted_mean,"../results/ensemble_weighted_mean.csv",row.names = F)  

f <- function(x){
  maximo=max(x)
  minimo=min(x)
  if(maximo>=(1-minimo)){
    return(maximo)
  }else{
    return(minimo)
  }
}

max_deviation_h1n1<-apply(datos_H1N1,1,f)
max_deviation_seas<-apply(datos_seas,1,f)

results_deviation = data.frame(respondent_id = c(26707:53414), h1n1_vaccine = 
                                 unname(max_deviation_h1n1), seasonal_vaccine = 
                                 unname(max_deviation_seas))
## Guardo los resultados
write.csv(results_deviation,"../results/ensemble_max_deviation.csv",row.names = F)  


######
f <- function(x){
  suma=sum(x>=0.5)
  maximo=max(x)
  minimo=min(x)
  if(suma>=3){
    return(maximo)
  }else{
    return(minimo)
  }
}

max_deviation_agreement_h1n1<-apply(datos_H1N1,1,f)
max_deviation_agreement_seas<-apply(datos_seas,1,f)

results_deviation_agreement = data.frame(respondent_id = c(26707:53414), h1n1_vaccine = 
                                     unname(max_deviation_h1n1), seasonal_vaccine = 
                                     unname(max_deviation_seas))
## Guardo los resultados
write.csv(results_deviation_agreement,"../results/ensemble_agreement_max_deviation.csv",row.names = F)  
