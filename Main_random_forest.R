rm(list=ls())
setwd("C:/Users/DEEKSHA/Desktop/edwisor/projects")

data1 = read.csv("TextClassification_Data.csv")

datatype_conv= function(data){
  
  data$SUMMARY = as.character(data$SUMMARY)
  data$previous_appointment = as.character(data$previous_appointment)
  data$sub_categories = as.character(data$sub_categories)
  data$categories = as.character(data$categories)
  data$DATA=as.character(data$DATA)
  return (data)
}

#data$ID=as.numeric(data$ID)
#str(data)
#table(data$sub_categories)
#length(table(data$sub_categories))
ini_pro= function(data){
  for (i in 1:nrow(data)) {
    data[i,6]=toupper(data[i,6])
    data[i,5]=toupper(data[i,5])
    data[i,4]=toupper(data[i,4])
    data[i,2]=tolower(data[i,2])
    data[i,3]=tolower(data[i,3])
  }
  return (data)
}



#replacing 2 missing values from var previous_appointment 
mv_rep= function(data){
  for (i in 1:nrow(data)) {
    if(data[i,6]==""){
      data[i,6]=NA
    }
  }
  for (i in 1:nrow(data)) {
    if(data[i,2]==""){
      data[i,2]=NA
    }
    
  }
  data=data[complete.cases(data),]
  return (data)
}

#rm(tdm, tdm2, postCorpus, i, regular_summary, regular1)

#For Data column
regular1=function(sr){
  sr=gsub("\\\\[^\\s]+\\s"," ",sr,perl=T)
  sr=gsub("x"," ",sr,perl=T)
  sr=gsub("}"," ",sr,perl=T)
  sr=gsub("{"," ",sr,perl=T)
  sr=gsub("-"," ",sr,perl=T)
  sr=gsub(";"," ",sr,perl=T)
  sr=gsub("ap[p,t,pt]+\\s","appointment ",sr,perl=T)
  sr=gsub("patient","pt ",sr,perl=T)
  sr=gsub("sch[\\s,ed,d]+\\s","schedule ",sr,perl=T)
  sr=gsub("cl[l,ld,ed,d]+\\s","called ",sr,perl=T)
  sr=gsub("cal[l,ld,ed,d]+\\s","called ",sr,perl=T)
  
  #r=gsub("\\scal"," called ",r,perl=T)
  #r=gsub("cal\\w+","called ",r,perl=T)
  sr=gsub("med[s,\\s,s,]+\\s","medicine ",sr,perl=T)
  sr=gsub("m[d,s]+\\s"," medicine ",sr,perl=T)
  #r=gsub("\\ssz"," seizure ",r,perl=T)
  sr=gsub("sz\\s","seizure ",sr,perl=T)
  sr=gsub("sz","seizure ",sr,perl=T)
  #r=gsub("\\sre"," regarding ",r,perl=T)
  #r=gsub("re\\s","regarding ",r,perl=T)
  #r=gsub("\\schk"," check ",r,perl=T)
  sr=gsub("chk\\s","check ",sr,perl=T)
  sr=gsub("spk\\s","speak ",sr,perl=T)
  sr=gsub("abt\\s","about ",sr,perl=T)
  sr=gsub("dr[.,\\s]+\\s","doctor ",sr,perl=T)
  #r=gsub("wt\\s","weight ",r,perl=T)
  sr=gsub("req\\s","request ",sr,perl=T)
  sr=gsub("pl[s,z]+\\s","please ",sr,perl=T)
  #r=gsub("pl\\s","please ",r,perl=T)
  sr=gsub("s/w\\s","speak with ",sr)
  
  
  sr=gsub("paragraph"," ",sr,perl=T)
  sr=gsub("/"," ",sr,perl=T)
  sr=gsub("\\\\"," ",sr,perl=T)
  sr=gsub("sscharaux"," ",sr,perl=T)
  sr=gsub("par"," ",sr,perl=T)
  sr=gsub("protect"," ",sr,perl=T)
  sr=gsub("wrote"," ",sr,perl=T)
  sr=gsub(":"," ",sr,perl=T)
  sr=gsub("[0-9]"," ",sr,perl=T)
  sr=gsub("pm"," ",sr,perl=T)
  sr=gsub("am"," ",sr,perl=T)
  sr=gsub("arial"," ",sr,perl=T)
  sr=gsub("font"," ",sr,perl=T)
  sr=gsub("normal"," ",sr,perl=T)
  sr=gsub("default"," ",sr,perl=T)
  sr=gsub("plain"," ",sr,perl=T)
  sr=gsub("home"," ",sr,perl=T)
  sr=gsub("phone"," ",sr,perl=T)
  sr=gsub("cell"," ",sr,perl=T)
  sr=gsub("called"," ",sr,perl=T)
  sr=gsub("note"," ",sr,perl=T)
  sr=gsub("fs"," ",sr,perl=T)
  sr=gsub("cf"," ",sr,perl=T)
  sr=gsub("\\s+", " ",sr,perl = T)
  sr=gsub("converted from flag"," ",sr,perl=T)
  sr=gsub("call[\\w+,\\s] taken by"," ",sr,perl=T)
  sr=gsub("phone note called pt back"," ",sr,perl=T)
  sr=gsub("phone note called"," ",sr,perl=T)
  sr=gsub("follow up details"," ",sr,perl=T)
  sr=gsub("\\s+", " ",sr,perl = T)
}
DATA_prepro= function(data){
  for (i in 1:nrow(data)) {
    data[i,3]=regular1(data[i,3])
    #print(post[i,2])
  }
  return (data)
}
regular_summary=function(r){
  #r=gsub("pt\\w+","patient ",r,perl=T)
  r=gsub("patient","pt ",r,perl=T)
  r=gsub("sch[\\s,ed,d]","schedule ",r,perl=T)
  r=gsub("cl[l,ld,ed,d]+\\s","called ",r,perl=T)
  r=gsub("cal[l,ld,ed,d]+\\s","called ",r,perl=T)
  r=gsub("ap[p,t,pt]\\s","appointment ",r,perl=T)
  #r=gsub("\\scal"," called ",r,perl=T)
  #r=gsub("cal\\w+","called ",r,perl=T)
  r=gsub("med[s,\\s]+\\s","medicine ",r,perl=T)
  r=gsub("m[d,s]+\\s"," medicine ",r,perl=T)
  #r=gsub("\\ssz"," seizure ",r,perl=T)
  r=gsub("sz\\s","seizure ",r,perl=T)
  r=gsub("sz","seizure ",r,perl=T)
  #r=gsub("\\sre"," regarding ",r,perl=T)
  #r=gsub("re\\s","regarding ",r,perl=T)
  #r=gsub("\\schk"," check ",r,perl=T)
  r=gsub("chk\\s","check ",r,perl=T)
  r=gsub("spk\\s","speak ",r,perl=T)
  r=gsub("abt\\s","about ",r,perl=T)
  r=gsub("dr[.,\\s]+\\s","doctor ",r,perl=T)
  #r=gsub("wt\\s","weight ",r,perl=T)
  r=gsub("req\\s","request ",r,perl=T)
  r=gsub("pl[s,z]+\\s","please ",r,perl=T)
  #r=gsub("pl\\s","please ",r,perl=T)
  r=gsub("s/w\\s","speak with ",r)
  r=gsub("confirm[//s,//w+]","confirm",r)
  r="discussion "
  r=gsub("discuss//w+//s","discuss",r,perl=T)
  r
}

SUMMARY_prepro= function(data){
  for (i in 1:nrow(data)) {
    data[i,2]=regular_summary(data[i,2])
    #print(post[i,2])
  }
  return (data)
}



data1=datatype_conv(data1)###data type conv
data1= ini_pro(data1)#####changing the case
data1= mv_rep(data1)#####removing missing values
data1= DATA_prepro(data1)#####string manipulation in DATA var
data1= SUMMARY_prepro(data1)#####string manipulation in SUMMARY var
data1$text = paste(data1$SUMMARY, data1$DATA, sep='.') 
#class(data1$text)
data1$SUMMARY=NULL
data1$DATA=NULL
data1$fileid=NULL
data1$ID=NULL

#data = read.csv("data3.csv")
#data$X=NULL
str(data)
#data1=data
#data1[1:2,1:2]
#freq=colSums(as.matrix(data[,4:ncol(data)]))
#ord=order(freq)
#length(freq)
#freq[tail(ord)]


library(stringr)
library(tm)
data1$text=as.character(data1$text)
data_corpus = VCorpus(VectorSource(data1$text)) #forming a corpus
data_corpus=tm_map(data_corpus, tolower)
data_corpus = tm_map(data_corpus,content_transformer(tolower))
data_corpus = tm_map(data_corpus,removePunctuation)  #removing punchuations
data_corpus = tm_map(data_corpus,removeNumbers) #removing numbers
#data_corpus = tm_map(data_corpus,tolower) #converting to lowercase
data_corpus = tm_map(data_corpus,removeWords,c('pt','able','its','it','us','use','used','using','will','yes','say','can','take','one',
                                               'adderall','adol','ago','answere','action','address','afternoon','anda','anything',
                                               'ada','alt', 'arround','asked', 'area', 'april', 'assoc', 'atp', 'call','caller','cma', 
                                               'comments', 'cook', 'copy', 'count', 'cover', 'daughter', 'dhe', 'harry', 'mom',
                                               'denise', 'done', 'holly', 'hollie', 'duke', 'first', 'iii', 'ily', 'ing', 'inj',
                                               'keep', 'let', 'like', 'linda', 'jones', 'mary', 'mother', 'pdf', 'wife', 'way', 'monday', 
                                               'monday', 'tuesday', 'wednesday', 'thursday', 'friday','saturday','sunday',
                                               'yesterday', 'today', 'tommorow','january', 'february', 'may','march','june','july','august',
                                               'september','october','november','december','wendy', stopwords('english'))) #removing english stopwords
data_corpus = tm_map(data_corpus,stripWhitespace) #removing the whitespaces
#data_corpus = tm_map(data_corpus,removeWords,stopwords("SMART"))
corpus_copy=data_corpus
data_corpus=tm_map(data_corpus, gsub, pattern = "appointments", replacement = "appointment")
data_corpus=tm_map(data_corpus, gsub, pattern = "appt", replacement = "appointment")
#data_corpus=tm_map(data_corpus, gsub, pattern = "", replacement = "appointment")

data_corpus = tm_map(data_corpus, PlainTextDocument)
tdm = t(TermDocumentMatrix(data_corpus))
#tdm2 <- removeSparseTerms(tdm, 0.98)
td3=removeSparseTerms(tdm, 0.99)

tdm_data_565=data.frame(as.matrix(td3))
final_data2=as.data.frame(cbind(data1$previous_appointment,tdm_data_565))
#names(final_data)

colnames(final_data)[1]="previous_appointment"


data=final_data2
#data$X=NULL
pre_modelling=function(final_data){
  for (i in 1:ncol(final_data)) {
    final_data[,i]=as.data.frame(as.factor(final_data[,i]))
    
  }
  return(final_data)
}

data=pre_modelling(data)

library(h2o)
library(mlbench)
library(caret)
localH2O = h2o.init(nthreads = -1)
h2o.init()
test.h2o = as.h2o(data)

#to predict the category
super_model = readRDS("./rf_predict_categories.rds")
predict.gbm = as.data.frame(h2o.predict(super_model, test.h2o))

#to predict the sub_category
test.h2o = as.h2o(data)
super_model = readRDS("./gbm_final_model_subcategory.rds")
predict.gbm = as.data.frame(h2o.predict(super_model, test.h2o))


