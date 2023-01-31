library(e1071)
library(caTools)
library(class)
library(neuralnet)
data0 = read.csv("milknew.csv",header = TRUE, 
stringsAsFactors = T)
idx=sample(1:nrow(data0),0.8*nrow(data0))
train=data0[idx,]
test=data0[-idx,]
head(data0)
model=neuralnet(Grade ~ . ,data=train, hidden = 7, 
		threshold=0.5, stepmax = 1e+06)
plot(model)
test[-8]
mypredict=compute(model,test[-8])$net.result
maxidx=function(x){
  return(which(x==max(x)))
}
idx=apply(mypredict,1,maxidx)
prediction=c('high','low','medium')[idx]
cm=table(prediction,test$Grade)
cm
sum(diag(cm))/sum(cm)