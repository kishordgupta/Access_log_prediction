library(caret)
library(tm)
library(naivebayes)
dataad <- read.csv("~/Rscripta/accesslogml_trainingNBD.csv")
dataa <- na.omit(dataad)



trainIndex=createDataPartition(dataa$result, p=0.7)$Resample1
train=na.omit(dataa[trainIndex, ])
test=na.omit(dataa[-trainIndex, ])


NBclassfier=naiveBayes(train$result~train$Dep+train$Exp+train$Workstation+train$filesize+train$filetype+train$Passingyear , data=train)

print(NBclassfier)
trainPred=predict(NBclassfier, data, type = "class")
trainTable=table(as.factor(train$result), trainPred)



