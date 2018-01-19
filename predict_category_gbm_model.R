rm(list=ls())
setwd("C:/Users/DEEKSHA/Desktop/edwisor/projects")

data=read.csv("final_data1.csv")
data$X=NULL
data$sub_categories=NULL

#to change the datatype of all the variables to factor or categorical
pre_modelling=function(final_data){
  for (i in 1:ncol(final_data)) {
    final_data[,i]=as.data.frame(as.factor(final_data[,i]))
    
  }
  return(final_data)
}

data=pre_modelling(data)
str(data$add)
test[1:3,1:3]
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
install.packages("h2o")
library(h2o)
library(mlbench)
library(caret)
localH2O = h2o.init(nthreads = -1)
h2o.init()
train.h2o = as.h2o(train)
test.h2o = as.h2o(test)
#dependent variable (Purchase)
y.dep = 1

#independent variables
x.indep = c(2:ncol(train.h2o))

gbm.model <- h2o.gbm(y=y.dep, x=x.indep, training_frame = train.h2o,
                     ntrees = 1000, max_depth = 4, learn_rate = 0.01, seed = 1122)
#save(gbm.model, file = "gbm_model.rda")
#mod=load('gbm_model.rda')
saveRDS(gbm.model, "./gbm_final_model.rds")
# load the model
#super_model <- readRDS("./gbm_final_model.rds")
#print(super_model)
h2o.performance(gbm.model)
h2o.varimp(gbm.model)
predict.gbm = as.data.frame(h2o.predict(gbm.model, test.h2o[-1]))


confusionMatrix(as.matrix(predict.gbm$predict),as.matrix(test.h2o$categories))


#h2o.confusionMatrix(xtab)
#xtab= table(as.vector(test.h2o[,1]), as.vector(predict.rforest[,1]))
#xtab

