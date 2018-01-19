#Histogram 
rm(list=ls())

setwd("C:/Users/DEEKSHA/Desktop/edwisor/projects")
data1 = read.csv("TextClassification_Data.csv")
data1= ini_pro(data1)#####changing the case
library("ggplot2")# to plot graphs
library("scales")#to set x and y axis limit
library("corrplot")##to plot the correlation plot
library("psych")###to adjust the fonts
library("gplots")###for color adjustment 
data1$ID= as.numeric(data1$ID)
ggplot(data1, aes_string(x = data1$fileid)) + 
  geom_histogram(fill="cornsilk", colour = "black") + geom_density() +
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  scale_x_continuous(breaks=pretty_breaks(n=10))+
  theme_bw() + xlab("Id") + ylab("Frequency") + ggtitle("ID") +
  theme(text=element_text(size=18))
ggsave("id_hist.png", width = 10 )


data1$sub_categories = as.factor(data1$sub_categories)
ggplot(data1, aes_string(x =data1$sub_categories , y = data1$ID, 
                          fill = data1$sub_categories)) + 
  geom_boxplot(outlier.colour = "red", outlier.size = 3) + 
  scale_y_continuous(breaks=pretty_breaks(n=10)) + 
  guides(fill=FALSE) + theme_bw() + xlab("Sub-Categories") + 
  ggtitle("Outlier Analysis") +  
  theme(text=element_text(size=15))
ggsave("sub_cat_boxplot.png")

table(data1$previous_appointment)
