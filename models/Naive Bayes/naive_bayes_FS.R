library(e1071)
library(caret)
library(pROC)
library(FSelectorRcpp)
source("./funciones.r")






cv_train_naive_bayes<-function(x_train,y_train,laplace=0,k_folds=10){
  
  fold_naive_bayes<-function(fold,data,labels,label_pos,laplace=0){
    pred_label=labels[label_pos]
    train_fold<-data[-fold,]
    test_fold<-data[fold,]
    nb <- naiveBayes(train_fold[,-c(labels)],train_fold[,pred_label],laplace=laplace)
    pred <- predict(nb,test_fold)
    accuracy <- sum(pred==test_fold[,pred_label])/length(fold)
    pred_prob <- predict(nb,test_fold,type="raw")
    auc <- roc(test_fold[,pred_label],pred_prob[,1],quiet=TRUE)$auc
    return(c(accuracy,auc))
  }
  
  
  y_united <- tidyr::unite(y_train, Y, c(h1n1_vaccine,seasonal_vaccine), sep="", remove = TRUE)
  y_united <- y_united %>% mutate_all(as.factor)
  db<-cbind(x_train,y_united)
  flds <- createFolds(db$Y, k = k_folds)
  accuracy_cv<-list(0,0)
  auc_cv<-list(0,0)
  
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



datos = leer_datos("../data/training_set_features_preprocessed.csv", 
                   "../data/training_set_labels.csv", 
                   "../data/test_set_features_preprocessed.csv", juntar_etiquetas = FALSE)

x_train <- datos[[1]]
y_train <- datos[[2]]
x_test <- datos[[3]]


atributos_h1n1 <- FSelectorRcpp::information_gain(h1n1_vaccine ~ ., 
                                                base::cbind(x_train,
                                                            y_train[,1]))

atributos_seasonal <- FSelectorRcpp::information_gain(seasonal_vaccine ~ ., 
                                                    base::cbind(x_train,
                                                                y_train[,2]))

atributos_h1n1=atributos_h1n1 %>% arrange(desc(importance))
atributos_seasonal=atributos_seasonal%>%arrange(desc(importance))

f<-function(n,features,laplace=0){
  f_trimmed<-head(features,n)$attributes
  cv_train_naive_bayes(x_train[,f_trimmed],y_train,laplace=laplace)
}


laplace_coef=1

## A continuaci?n est? comentado porque la ejecuci?n tarda mucho y se guardan los resultados.

#fs_h1n1<-lapply(1:dim(x_train)[2],function (n) f(n,atributos_h1n1,laplace=laplace_coef))

#auc_h1n1_fs<-sapply(fs_h1n1,function(x) x$h1n1$AUC)
#print(which.max(auc_h1n1_fs))
#print(max(auc_h1n1_fs))
#ggplot()+aes(x=1:length(auc_h1n1_fs),y=auc_h1n1_fs)+geom_line(color="red")+geom_point()+labs(x="Atributos utilizados",y="AUC")+ggtitle("AUC para H1N1")

#fs_seasonal<-lapply(1:dim(x_train)[2],function (n) f(n,atributos_seasonal,laplace=laplace_coef))

#auc_seasonal_fs<-sapply(fs_seasonal,function(x) x$seasonal$AUC)
#print(which.max(auc_seasonal_fs))
#print(max(auc_seasonal_fs))
#ggplot()+aes(x=1:length(auc_seasonal_fs),y=auc_seasonal_fs)+geom_line(color="red")+geom_point()+labs(x="Atributos utilizados",y="AUC")+ggtitle("AUC para Vacuna Estacional")

write.csv(auc_h1n1_fs,'./features/h1n1_fs_l1.csv',row.names = FALSE, col.names=FALSE)
write.csv(auc_seasonal_fs,'./features/seasonal_fs_l1.csv',row.names = FALSE, col.names=FALSE)


#auc_h1n1_fs=read.csv('./features/h1n1_fs_l1.csv')$x
#auc_seasonal_fs=read.csv('./features/seasonal_fs_l1.csv')$x

ggplot()+aes(x=2:length(auc_h1n1_fs),y=auc_h1n1_fs)+geom_line(color="red")+geom_point()+labs(x="Atributos utilizados",y="AUC")+ggtitle("AUC para H1N1, IG")
ggplot()+aes(x=2:length(auc_seasonal_fs),y=auc_seasonal_fs)+geom_line(color="red")+geom_point()+labs(x="Atributos utilizados",y="AUC")+ggtitle("AUC para Vacuna Estacional, IG ")
### Nuevo orden seg?n los resultados
new_order_h1n1=c(1,order(diff(auc_h1n1_fs),decreasing=TRUE)+1)
new_order_seasonal=c(1,order(diff(auc_seasonal_fs),decreasing=TRUE)+1)

feat_h1n1_new_order=atributos_h1n1[new_order_h1n1,]
feat_seasonal_new_order=atributos_seasonal[new_order_seasonal,]

fs_h1n1_reord<-lapply(2:dim(x_train)[2],function (n) f(n,feat_h1n1_new_order,laplace=laplace_coef))

auc_h1n1_fs_reord<-sapply(fs_h1n1_reord,function(x) x$h1n1$AUC)
print(which.max(auc_h1n1_fs_reord))
print(max(auc_h1n1_fs_reord))
ggplot()+aes(x=1:length(auc_h1n1_fs_reord),y=auc_h1n1_fs_reord)+geom_line(color="red")+geom_point()+labs(x="Atributos utilizados",y="AUC")+ggtitle("AUC para H1N1, Reordenados")

fs_seasonal_reord<-lapply(2:dim(x_train)[2],function (n) f(n,feat_seasonal_new_order,laplace=laplace_coef))

auc_seasonal_fs_reord<-sapply(fs_seasonal_reord,function(x) x$seasonal$AUC)
print(which.max(auc_seasonal_fs_reord))
print(max(auc_seasonal_fs_reord))
ggplot()+aes(x=1:length(auc_seasonal_fs_reord),y=auc_seasonal_fs_reord)+geom_line(color="red")+geom_point()+labs(x="Atributos utilizados",y="AUC")+ggtitle("AUC para Vacuna Estacional, Reordenados")

write.csv(auc_h1n1_fs_reord,'./features/h1n1_fs_l1_reordered.csv',row.names = FALSE, col.names=FALSE)
write.csv(auc_seasonal_fs_reord,'./features/seasonal_fs_l1_reordered.csv',row.names = FALSE, col.names=FALSE)

#auc_h1n1_fs_reord=read.csv('./features/h1n1_fs_l1.csv')$x
#auc_seasonal_fs_reord=read.csv('./features/seasonal_fs_l1.csv')$x

write.csv(feat_h1n1_new_order[1:which.max(auc_h1n1_fs_reord),]$attributes,'./features/fs_naive_bayes_h1n1.csv',row.names=FALSE,col.names=FALSE)
write.csv(feat_seasonal_new_order[1:which.max(auc_seasonal_fs_reord),]$attributes,'./features/fs_naive_bayes_seasonal.csv',row.names=FALSE,col.names=FALSE)
