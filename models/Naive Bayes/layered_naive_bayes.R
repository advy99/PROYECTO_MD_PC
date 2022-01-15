library(e1071)
library(caret)
library(pROC)
library(FSelectorRcpp)
source("../funciones.r")
cv_train_naive_bayes_2f<-function(x_train,y_train,attrib_total,attrib_missm,label=1,alpha=0.1,laplace=0,k_folds=10){
  
  # Esta funci?n auxiliar entrena naive bayes en un fold de CV
  fold_naive_bayes<-function(fold,data,labels,label_pos,attrib_missm,alpha=0.1,laplace=0){
    #Separo train y test seg?n la partici?n de la validaci?n cruzada.
    pred_label=labels[label_pos]
    train_fold<-data[-fold,]
    test_fold<-data[fold,]
    
    #Entreno naive bayes
    nb1 <- naiveBayes(train_fold[,-c(labels)][,attrib_total],train_fold[,pred_label],laplace=laplace)
    
    #Calculo la predicci?n de clases para accuracy
    pred <- predict(nb1,test_fold)
    missmatch<-pred!=test_fold[,pred_label]
    
    nb2 <-  naiveBayes(train_fold[missmatch,-c(labels)][,attrib_missm],train_fold[missmatch,pred_label],laplace=laplace)
    
    
    pred_prob_1 <- predict(nb1,test_fold,type="raw")[,2]
    pred_prob_2 <- predict(nb2,test_fold,type="raw")[,2]
    
    pred_prob <- pred_prob_1*(1-alpha)+pred_prob_2*alpha
    
    ##Calculo la probabilidad de pertenencia a cada clase seg?n naive bayes para AUC
    auc <- roc(test_fold[,pred_label],pred_prob,quiet=TRUE)$auc
    return(c(0,auc))
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
  
  results<-lapply(flds,function(fold) fold_naive_bayes(fold,data,labels,label,laplace))
  
  
  Acc <-mean(sapply(results,function (x) x[1]))
  Auc<-mean(sapply(results,function (x) x[2]))
  metrics=list(Accuracy=Acc,AUC=Auc)
  
  return(metrics)
}

datos = leer_datos("../../data/training_set_features_preprocessed.csv", 
                   "../../data/training_set_labels.csv", 
                   "../../data/test_set_features_preprocessed.csv", juntar_etiquetas = FALSE)

x_train <- datos[[1]]
y_train <- datos[[2]]
x_test <- datos[[3]]

#Lectura de atributos
h1n1_attrib<-read.csv('./features/fs_naive_bayes_h1n1.csv',header=T)[[1]]
seasonal_attrib<-read.csv('./features/fs_naive_bayes_seasonal.csv',header=F)[[1]]
h1n1_miss_attrib<-read.csv('./features/fs_naive_bayes_miss_h1n1.csv')[[1]]
seasonal_miss_attrib<-read.csv('./features/fs_naive_bayes_miss_seasonal.csv')[[1]]


h1n1_train<-x_train[,h1n1_attrib]
seasonal_train<-x_train[,seasonal_attrib]

laplace=1
alpha=0.1

### H1N1

#Se entrena el primer modelo
nb_h1n1_pre <- naiveBayes(x_train[,h1n1_attrib],y_train$h1n1_vaccine,laplace=laplace)
#Se calculan los errores
missmatch_h1n1<-predict(nb_h1n1_pre,x_train)!=y_train$h1n1_vaccine
#Se entrena el modelo con los errores
nb_h1n1_post<-naiveBayes(x_train[missmatch_h1n1,h1n1_miss_attrib],y_train$h1n1_vaccine[missmatch_h1n1],laplace=laplace)

#Se combinan ambos
pred_h1n1<-predict(nb_h1n1_pre,x_test,type="raw")[,2]*(1-alpha)
pred_h1n1<-pred_h1n1+predict(nb_h1n1_post,x_test,type="raw")[,2]*alpha

### Seasonal

#Se entrena el primer modelo
nb_seasonal_pre <- naiveBayes(x_train[,seasonal_attrib],y_train$seasonal_vaccine,laplace=laplace)
#Se calculan los errores
missmatch_seasonal<-predict(nb_seasonal_pre,x_train)!=y_train$seasonal_vaccine
#Se entrena el segundo modelo con los errores
nb_seasonal_post<-naiveBayes(x_train[missmatch_seasonal,seasonal_miss_attrib],y_train$seasonal_vaccine[missmatch_seasonal],laplace=laplace)

#Se combinan ambos
pred_seasonal<-predict(nb_seasonal_pre,x_test,type="raw")[,2]*(1-alpha)
pred_seasonal<-pred_seasonal+predict(nb_seasonal_post,x_test,type="raw")[,2]*alpha

###########

resultados = data.frame(respondent_id = c(26707:53414), h1n1_vaccine = 
                          unname(pred_h1n1), seasonal_vaccine = 
                          unname(pred_seasonal))

write.csv(resultados,"../../results/layered_bayes_results.csv",row.names = F)  