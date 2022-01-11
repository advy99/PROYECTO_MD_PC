library(e1071)
library(caret)
library(pROC)
library(FSelectorRcpp)
source("funciones.r")

####

# Naive Bayes

####


###

#Funciones

###

##
# Esta funci?n eval?a un modelo de naive bayes con los datos proporcionados
# Solo sirve para datos con las etiquetas h1n1 vaccine y seasonal vaccine.
# Devuelve el AUC y la precisi?n media
##
cv_train_naive_bayes<-function(x_train,y_train,laplace=0,k_folds=10){
  
  # Esta funci?n auxiliar entrena naive bayes en un fold de CV
  fold_naive_bayes<-function(fold,data,labels,label_pos,laplace=0){
    #Separo train y test seg?n la partici?n de la validaci?n cruzada.
    pred_label=labels[label_pos]
    train_fold<-data[-fold,]
    test_fold<-data[fold,]
    
    #Entreno naive bayes
    nb <- naiveBayes(train_fold[,-c(labels)],train_fold[,pred_label],laplace=laplace)
    
    #Calculo la predicci?n de clases para accuracy
    pred <- predict(nb,test_fold)
    accuracy <- sum(pred==test_fold[,pred_label])/length(fold)
    
    #Calculo la probabilidad de pertenencia a cada clase seg?n naive bayes para AUC
    pred_prob <- predict(nb,test_fold,type="raw")
    auc <- roc(test_fold[,pred_label],pred_prob[,1],quiet=TRUE)$auc
    return(c(accuracy,auc))
  }
  
  # Hago los folds juntando las dos etiquetas para que mantenga la proporcion en ambas clases
  
  y_united <- tidyr::unite(y_train, Y, c(h1n1_vaccine,seasonal_vaccine), sep="", remove = TRUE)
  y_united <- y_united %>% mutate_all(as.factor)
  db<-cbind(x_train,y_united)
  flds <- createFolds(db$Y, k = k_folds)
  accuracy_cv<-list(0,0)
  auc_cv<-list(0,0)
  
  #Junto los datos y las etiquetas por simplicidad
  data<- cbind(x_train,y_train)
  
  labels<-c(dim(data)[2]-1,dim(data)[2])
  
  h1n1<-lapply(flds,function(fold) fold_naive_bayes(fold,data,labels,1,laplace))
  seasonal<-lapply(flds,function(fold) fold_naive_bayes(fold,data,labels,2,laplace))
  
  
  Acc_h1n1 <-mean(sapply(h1n1,function (x) x[1]))
  Auc_h1n1<-mean(sapply(h1n1,function (x) x[2]))
  h1n1_metrics=list(Accuracy=Acc_h1n1,AUC=Auc_h1n1)
  
  Acc_seasonal<-mean(sapply(seasonal,function (x) x[1]))
  Auc_seasonal<-mean(sapply(seasonal,function (x) x[2]))
  seasonal_metrics<-list(Accuracy=Acc_seasonal,AUC=Auc_seasonal)
  
  return(list(h1n1=h1n1_metrics,seasonal=seasonal_metrics))
}


## Lectura de datos ya preprocesados

datos = leer_datos("../data/training_set_features_preprocessed.csv", 
                   "../data/training_set_labels.csv", 
                   "../data/test_set_features_preprocessed.csv", juntar_etiquetas = FALSE)

x_train <- datos[[1]]
y_train <- datos[[2]]
x_test <- datos[[3]]


## Sin feature selection
#res_lp1<-cv_train_naive_bayes(x_train,y_train,laplace=1)




## Despu?s de feature selection (en naive_vayes_FS.R)
h1n1_attrib<-read.csv('../executions/fs_naive_bayes_h1n1.csv',header=F)[[1]]
seasonal_attrib<-read.csv('../executions/fs_naive_bayes_seasonal.csv',header=F)[[1]]

h1n1_train<-x_train[,h1n1_attrib]
seasonal_train<-x_train[,seasonal_attrib]

nb_h1n1<-naiveBayes(h1n1_train,y_train$h1n1_vaccine,laplace=1)

nb_seasonal<-naiveBayes(seasonal_train,y_train$seasonal_vaccine,laplace=1)

h1n1_test<-x_test[,h1n1_attrib]
seasonal_test<-x_test[,seasonal_attrib]



preds_h1n1<-predict(nb_h1n1,h1n1_test,type="raw")
preds_seasonal<-predict(nb_seasonal,seasonal_test,type="raw")

resultados = data.frame(respondent_id = c(26707:53414), h1n1_vaccine = 
                          unname(preds_h1n1[,2]), seasonal_vaccine = 
                          unname(preds_seasonal[,2]))

## Guardo los resultados
#write.csv(resultados,"../results/Bayes_Basic_FS_results.csv",row.names = F)  


#####

# Analisis de cambios de probabilidad

#####

#Ordena las variables seg?n el cambio de probabilidad de las clases seg?n naive bayes.
analizar_informacion_naive_bayes<-function(modelo_nb,umbral=0){
  f<-function(x){
    f2<-function(z){
      if(sum(z)>umbral){
        return(diff(z)/sum(z))
      }
      else{
        return(0)
      } 
    }
    apply(x,2,f2)
  }
  analisis<-lapply(modelo_nb$tables,f)
  #print(analisis)
  informacion<-sapply(analisis,function(x) max(abs(x)))
  informacion_min<-sapply(analisis,function(x) min(abs(x)))
  ordenados<-order(informacion,decreasing=TRUE)
  ordenados_min<-order(informacion_min,decreasing=T)
  list(Raw=analisis,Max=informacion[ordenados],Min=informacion_min[ordenados_min])
}
nb_total_h1n1<-naiveBayes(x_train,y_train$h1n1_vaccine,laplace=1)
analizar_informacion_naive_bayes(nb_total_h1n1)
nb_total_seasonal<-naiveBayes(x_train,y_train$seasonal_vaccine,laplace=1)
analizar_informacion_naive_bayes(nb_total_seasonal)

#ggplot()+aes(x=names(analizar_informacion_naive_bayes(nb_total_seasonal)),y=analizar_informacion_naive_bayes(nb_total_seasonal))+geom_bar()

###
# Peque?o an?lisis de resultados confusos
###

# Muestro los resultados que est?n en torno a 0.5.
# Esos son los datos que Naive Bayes no puede clasificar correctamente al no tener suficiente potencia

epsilon=0.2

preds_train_h1n1<-predict(nb_h1n1,h1n1_train,type="raw")
problematicos_h1n1<-preds_train_h1n1[,2]<(0.5+epsilon) & preds_train_h1n1[,2]>(0.5-epsilon)
sum(problematicos_h1n1)

preds_train_seas<-predict(nb_seasonal,seasonal_train,type="raw")
problematicos_seas<-preds_train_seas[,2]<(0.5+epsilon) & preds_train_seas[,2]>(0.5-epsilon)
sum(problematicos_seas)

problematicos_ambos<-problematicos_h1n1 & problematicos_seas
sum(problematicos_ambos)

data=cbind(x_train,y_train)

summary(data)
summary(data[problematicos_h1n1,])
summary(data[problematicos_seas,])
summary(data[problematicos_ambos,])

