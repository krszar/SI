library(party)
library(partykit)
library(e1071)
library(caTools)
library(class)
library(neuralnet)
car = read.csv("milknew.csv",header = TRUE,
	stringsAsFactors = T)

idx=sample(2,150,replace = T, prob=c(0.8,0.2))

train = car[idx == 1,]
test = car[idx ==2,]

model = ctree(Grade ~ . , data=train)
plot(model)

p = predict(model, test)

length(car$Grade)
cf = table (p, test$Grade)
cf
sum (diag ( cf )/sum( cf ))