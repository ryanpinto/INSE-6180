#rm(list=ls(all=TRUE))
#dat2 <- data.frame(lapply(data, function(x) as.numeric(as.character(x))))  
#apply(dat2, 2, function(x) any(is.na(x)))


library(readr)       #1
data <- read_csv("C:/Users/ryanp/Desktop/data.csv")
str(data)
attach(data)




norm=function(x){ return((x-min(x))/(max(x)-min(x)))}  #2
data_n=as.data.frame(lapply(data[,c(-1)],norm))
str(data_n)
summary(data_n)
set.seed(125)      #3
data_train=data_n[1:380,]
data_test=data_n[381:569,]
data_train_target=data[1:380,1]
data_test_target=data[381:569,1]
require(class)
k1=knn(train=data_train, test=data_test,cl=data_train_target$diagnosis,k=23 )

table(data_test_target$diagnosis,k1)


xtab <- table(data_test_target$diagnosis,k1) #4
library(caret)
confusionMatrix(xtab)

# 10 k cross validation using caret
train_control<- trainControl(method="cv", number=10, savePredictions = TRUE)
model<- train(as.factor(diagnosis)~., data=data, trControl=train_control, method="knn")
print(model)




#dim(train)
#dim(test)
#length(train_target)

