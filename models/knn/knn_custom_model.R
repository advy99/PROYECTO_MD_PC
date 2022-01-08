library(philentropy)
library(tidyverse)



my_knn <- function(train, train_labels, test, k=1, metric="euclidean"){

    print("Calculando distancias...")
    distancias <- as.data.frame(distance(as.data.frame(rbind(train,test)),
                                         mute.message = F, method = metric))
    distancias <- distancias %>% slice(dim(train)[1]+1:dim(distancias)[1])
    print("Distancias calculadas")
    
  
    print("Calculando clases...")

    ordenadas <- t(apply(distancias,1,order))

    ganadores <- t(apply(ordenadas,1, function(a) a[2:k+1]))
    
    etiquetas <- t(apply(ganadores,1,function(a) train_labels[a]))
    
    print("Calculando predicciones")

    #Necesitamos devolver las probabilaides es decir 
    print("Calculand pred0")
    pred_0 <- as.numeric(array(apply(etiquetas, 1, function(a) sum(a==0)/k)))
    print("Calculand pred1")
    pred_1 <- as.numeric(array(apply(etiquetas, 1, function(a) sum(a==1)/k)))
    print("Calculando clases")
    ret <- array(apply(etiquetas, 1, function(a) names(which.max(table(a)))))
    
    print("Clases calculadas")
    
    return(tibble(pred_1=pred_1, 
                  pred_0=pred_0,
                  class=ret))
}
