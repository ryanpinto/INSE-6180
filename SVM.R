#rm(list = ls(all=TRUE))

library(readr)
d<-read_csv("D:/DM/brst.csv")
str(d)
library(e1071)
dim(d)
names(d)

library(e1071)
dim(brst)

library(caret)

set.seed(123)
s<-sample(699,400)

#col<-c("clump" ,"size","adhesion" ,"nuclei", "nucleoli" , "benormal")

col<-c("size","benormal","chromatin","mitosis") 

#seprating test data and training data
tr_data<-brst[s,col]
ts_data<-brst[-s,col]

attach(brst)


svmfit <- svm(factor(benormal)~. , data=tr_data, kernel = "polynomial", cost = 10,scale = FALSE)
summary(svmfit)

plot(svmfit, BC1,size~chromatin,slice = list(size = 3, chromatin = 1))

tuned <- tune(svm, as.factor(benormal) ~ ., data=tr_data, kernel = "polynomial", ranges = list(cost = c(1,10,100)))
summary(tuned)

p<-predict(svmfit, ts_data[,col], type = "class")
tab<-table(predicted=p,actual=ts_data$benormal)
tab


#accuracy
mean(p == ts_data$benormal)
#misclassification
1-sum(diag(tab))/sum(tab)

confusionMatrix(p,ts_data$ benormal)

system.time(svmfit<-svm(factor(benormal)~. , data=tr_data, kernel = "polynomial", cost = 10,scale = FALSE))


