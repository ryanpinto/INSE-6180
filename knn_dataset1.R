#rm(list=ls(all=TRUE))
#summary(data)

library(readr)       #1 load data
data <- read_csv("C:/Users/ryanp/Desktop/brst.csv")
str(data)

dat2 <- data.frame(lapply(data, function(x) as.numeric(as.character(x))))  #2 convert to numeric
apply(dat2, 2, function(x) any(is.na(x)))



require(class)
set.seed(124)          #3 set seed and divide into test and train
ind=sample(2,nrow(dat2),replace=TRUE,prob=c(0.7,0.3))
train=dat2[ind==1,-10]
test=dat2[ind==2,-10]
cl<-dat2[ind==1,10]   
k1=knn(train=train, test=test,cl=cl,k=26 )


test2=dat2[ind==2,10] #4 
table(test2,k1)   #5


#dim(train)
#dim(test)
#length(train_target)

xtab <- table(test2, k1)
library(caret)
confusionMatrix(xtab)

# 10 k cross validation using caret
train_control<- trainControl(method="cv", number=10, savePredictions = TRUE)
model<- train(as.factor(benormal)~., data=data, trControl=train_control, method="knn")
print(model)
