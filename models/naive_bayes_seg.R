library(e1071)
library(caret)
setwd("D:\\Master\\Preprocesamiento y Clasificación\\Trabajo en grupo\\PROYECTO_MD_PC\\models")
source("funciones.r")






cv_train_naive_bayes2<-function(x_train,y_train,laplace=0,k_folds=10){
  fold_naive_bayes<-function(fold,data,labels,label_pos,laplace=0){
    pred_label=labels[label_pos]
    train_fold<-data[-fold,]
    test_fold<-data[fold,]
    nb <- naiveBayes(train_fold[,-c(labels)],train_fold[,pred_label])
    pred <- predict(nb,test_fold)
    accuracy <- sum(pred==test_fold[,pred_label])/length(test_fold)
    pred_prob <- predict(nb,test_fold,type="raw")
    auc <- roc(test_fold[,pred_label],pred_prob[,1],quiet=TRUE)$auc
    return(c(accuracy,auc))
  }
  
  
  y_united <- tidyr::unite(y_train, Y, c(h1n1_vaccine,seasonal_vaccine), sep="", remove = TRUE)
  y_united <- y_united %>% mutate_all(as.factor)
  db<-cbind(x_train,y_united)
  flds <- createFolds(db$Y, k = k_folds)
  accuracy_cv=list(0,0)
  auc_cv=list(0,0)
  
  data<- cbind(x_train,y_train)
  
  labels<-c(dim(data)[2]-1,dim(data)[2])
  
  
  print(lapply(flds,function(fold) fold_naive_bayes(fold,data,labels,1,laplace)))
  print(lapply(flds,function(fold) fold_naive_bayes(fold,data,labels,2,laplace)))
}



cv_train_naive_bayes<-function(x_train,y_train,laplace=0,k_folds=10){
  

  
  
  y_united <- tidyr::unite(y_train, Y, c(h1n1_vaccine,seasonal_vaccine), sep="", remove = TRUE)
  y_united <- y_united %>% mutate_all(as.factor)
  db<-cbind(x_train,y_united)
  flds <- createFolds(db$Y, k = k_folds)
  accuracy_cv=list(0,0)
  auc_cv=list(0,0)
  
  data<- cbind(x_train,y_train)
  
  label_1<-dim(data)[2]-1
  label_2<-dim(data)[2]
  
  for( fold in flds) {
    train_fold<-data[-fold,]
    test_fold<-data[fold,]
    nb <- naiveBayes(train_fold[,-c(label_1,label_2)],train_fold[,label_1])
    pred <- predict(nb,test_fold)
    accuracy_cv[[1]] <- accuracy_cv[[1]]+sum(pred==test_fold[,label_1])/length(test_fold)/k_folds
    pred_prob <- predict(nb,test_fold,type="raw")
    auc_cv[[1]] <- auc_cv[[1]]+roc(test_fold[,label_1],pred_prob[,1],quiet=TRUE)$auc/k_folds
  }
  
  for( fold in flds) {
    train_fold<-data[-fold,]
    test_fold<-data[fold,]
    nb <- naiveBayes(train_fold[,-c(label_1,label_2)],train_fold[,label_2])
    pred <- predict(nb,test_fold)
    accuracy_cv[[2]] <- accuracy_cv[[2]]+sum(pred==test_fold[,label_2])/length(test_fold)/k_folds
    pred_prob <- predict(nb,test_fold,type="raw")
    auc_cv[[2]] <- auc_cv[[2]]+roc(test_fold[,label_2],pred_prob[,1],quiet=TRUE)$auc/k_folds
  }
  return(list(Accuracy=accuracy_cv,AUC=auc_cv))
}


datos = leer_datos("../data/training_set_features_preprocessed.csv", 
                   "../data/training_set_labels.csv", 
                   "../data/test_set_features_preprocessed.csv", juntar_etiquetas = FALSE)

x_train = datos[[1]]
y_train = datos[[2]]
x_test = datos[[3]]


cv_train_naive_bayes2(x_train,y_train)
