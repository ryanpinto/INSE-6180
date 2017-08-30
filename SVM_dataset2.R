#rm(list = ls(all=TRUE))
library(readr)
d<-read_csv("D:/DM/data.csv")
str(d)
library(e1071)
dim(data)
names(data)
dim(data)
library(caret)

set.seed(123)
s<-sample(569,469)

#col<-c("clump" ,"size","adhesion" ,"nuclei", "nucleoli" , "benormal")
col<-c("radiusmean","diagnosis","texturemean") 

#seprating test data and training data


tr_data<-data[s,col]
ts_data<-data[-s,col]

attach(BC1)

svmfit <- svm(factor(diagnosis)~. , data=tr_data, kernel = "linear", cost = .1)
summary(svmfit)

plot(svmfit,tr_data,radiusmean~texturemean, slice = list(radiusmean = 5, texturemean = 24))
#plot(svmfit,data,radiusmean~texturemean,slice = list(perimetermean=153,areamean=462))


tuned <- tune(svm, as.factor(diagnosis) ~ ., data=tr_data, kernel = "linear", ranges = list(cost = c(1,10,100)))
summary(tuned)


p<-predict(svmfit, ts_data[,col], type = "class")
tab<-table(predicted=p,actual=ts_data$diagnosis)
tab


#accuracy
mean(p == ts_data$diagnosis)
#misclassification
1-sum(diag(tab))/sum(tab)

confusionMatrix(p,ts_data$diagnosis)

system.time(svm(factor(diagnosis)~. , data=tr_data, kernel = "linear", cost = .1))
