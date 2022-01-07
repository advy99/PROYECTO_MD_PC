require(tidyverse)

data<-read.csv('./data/training_set_features.csv',strip.white = T)

summary(data)
apply(data[1,],2,is.integer)
typeof(data$household_adults)
caracteres<-sapply(data[1,],is.character)
data<-apply(data,2,as.factor)
summary(data)
sapply(data[1,],is.numeric)
