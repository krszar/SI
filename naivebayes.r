library(naivebayes)
library(mlbench)
library(e1071)
library(data.table)
dane = read.csv("milknew.csv",header = TRUE, 
stringsAsFactors = T)

View(dane)
xtabs(~Grade, data=dane)
idx = sample(2, 163, replace = T, prob=c(0.8,0.2) )

str(dane)
train = dane[idx == 1,]
test = dane[idx ==2,]

model = naive_bayes(as.factor(Grade) ~ .,data=train, 
usekernel = T)
plot(model)
p = predict(model, test)
cf=table(p, test$Grade)
cf
cat("poprawność", sum(diag(cf))/sum(cf)*100, "%")