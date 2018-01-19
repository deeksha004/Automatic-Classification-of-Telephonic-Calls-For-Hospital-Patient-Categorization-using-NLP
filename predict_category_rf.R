rm(list=ls())
setwd("C:/Users/DEEKSHA/Desktop/edwisor/projects")

data=read.csv("final_data_565.csv")
data$X=NULL
data$sub_categories=NULL
pre_modelling=function(final_data){
  for (i in 1:ncol(final_data)) {
    final_data[,i]=as.data.frame(as.factor(final_data[,i]))
    
  }
  return(final_data)
}

data=pre_modelling(data)
str(data$add)
test[1:3,1:4]
#Divide the data into train and test
#random sampling
#library(sampling)
#test = data[sample(nrow(data), 5393,replace = F),]
#test=data[1:100,]
#train = data[!(1:nrow(data)) %in% as.numeric(row.names(test)), ]

#stratified sampling
set.seed(123)
library(caTools)
smpl = sample.split(data[,ncol(data)], 0.9) 
train = subset(data, smpl == T ) 
test = subset(data, smpl == F ) 



#Apply random forest model
#install.packages("h2o")
library(h2o)
library("randomForest")
library(mlbench)
library(caret)
localH2O = h2o.init(nthreads = -1)
h2o.init()
train.h2o <- as.h2o(train)
test.h2o = as.h2o(test)
#dependent variable (Purchase)
y.dep = 1

#independent variables 
x.indep = c(2:ncol(train.h2o))
rforest.model = h2o.randomForest(y=y.dep, x=x.indep,
                                 training_frame = train.h2o, ntrees = 1000,
                                 mtries = 3, max_depth = 4, seed = 1122)
h2o.performance(rforest.model)
h2o.varimp(rforest.model)
#super_model <- readRDS("./rf_predict_categories.rds")
predict.rforest = as.data.frame(h2o.predict(rforest.model, test.h2o[-1]))
#h2o.confusionMatrix(predict.rforest[,1], test.h2o[,1])
confusionMatrix(as.matrix(predict.rforest$predict),as.matrix(test.h2o$categories))
saveRDS(rforest.model, "./rf_predict_categories.rds")
