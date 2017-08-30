#to clear the Global Environment at the right 
#rm(list=ls(all=TRUE))

#install.packages("kernlab")
library(kernlab)

library(readr)
d<-read_csv("C:/Users/ryanp/Desktop/data.csv")
str(d)

#defined kernel function
kfunction <- function(linear =0, quadratic=0)
{
  k <- function (x,y)
  {
    linear*sum((x)*(y)) + quadratic*sum((x^2)*(y^2))
  }
  class(k) <- "kernel"
  k
}

#Partition the data int train(trd) and test(tsd) data
s<-sample(569,400)
s

trd<-data[s,]
tsd<-data[-s,]

attach(data)

#create new variables to represent the points in the form of (a1,a2),(b1,b2)
n = 400
x1<-c(trd$radiusmean)
a1<-x1
a1
#for normal distribution of the values
a2 = rnorm(a1,mean = 0,sd = 1)
a2
x2<-c(trd$areamean)
b1<-x2
#for normal distribution of the values
b2 = rnorm(b1,mean = 0,sd = 1)

#binding the data points collectively for combined representation
x = rbind(matrix(cbind(a1,a2),,2),matrix(cbind(b1,b2),,2))
#representing each point in the form of support vectors that is either positive or negative through replication
y <- matrix(c(rep(1,n),rep(-1,n)))

svp <- ksvm(x,y,type="C-svc",C = 0.1, kernel=kfunction(1,0),scaled=c())
#generate a contour plot based on the defined parameters
plot(c(max(x[,1]), max(x[,2])),c(min(x[,1]), min(x[,2])),xlab='Radius_Mean',ylab='Area_Mean',xlim=c(0,1500),ylim=c(-2.1,2.0))

#plot(svp,data=x,xlim=c(-1,2),ylim=c(.5,7.5))
#help("plot,ksvm-method")
#help("plot.ksvm")
#help("rbind")
#help("rep")
#help("ymatrix")
#help("points")
#help("coef")
#help("colSums")


#set the title of the plot
title(main='Linear Separable Features')
#get the actual support vectors in list form
ymat <- ymatrix(svp)

#project the points on the plot based on 
# 1. the weight associated with the each point and 2. the index of the support vector
points(x[-SVindex(svp),1], x[-SVindex(svp),2],col = "cyan", pch = ifelse(ymat[-SVindex(svp)] < 0, 2, 1))
points(x[SVindex(svp),1], x[SVindex(svp),2], col = "red",pch = ifelse(ymat[SVindex(svp)] < 0, 17, 16))

#help("points")
# Extract w and b(bias) from the model   
w <- colSums(coef(svp)[[1]] * x[SVindex(svp),])
b <- b(svp)

# Draw the lines
abline(b/w[2],-w[1]/w[2])
abline((b+1)/w[2],-w[1]/w[2],lty=2)
abline((b-1)/w[2],-w[1]/w[2],lty=2)


p<-predict(svp,x)
p 

tab<-table(predicted=p,y)

#tab<-table(predicted=p,actual=tsd$benormal)
#tab


#accuracy
mean(p == ymat)
#misclassification
1-sum(diag(tab))/sum(tab) 