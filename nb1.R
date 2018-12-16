library(e1071)
accesslogml_training <- read.csv("C:/Users/kishor/OneDrive - The University of Memphis/Data_SCI_FINAL/training.csv")
data<-accesslogml_training
data<-na.omit(accesslogml_training)
datatest<-read.csv("~/Rscripta/accesslogml_test.csv")




Naive_Bayes_Model=naiveBayes(data$Result ~ data$Amm +data$I1 + data$I2 + data$I3 + data$I4 + 
                               data$Sprc + data$Contract + data$Dep
                             + data$SPECcode + data$SpecNum+ data$PRC, ,usekernel=F, data=as.data.frame(data))

Naive_Bayes_Model
NB_Predictions= predict(Naive_Bayes_Model,data[1:100,], type = c("class", "raw"),threshold = 0.001, eps = 0)



table(data$Result,NB_Predictions)
library(mlr)
task = makeClassifTask(train,target = "result")

#Initialize the Naive Bayes classifier
selected_model = makeLearner("classif.naiveBayes")

#Train the model
NB_mlr = train(selected_model, task)

#Read the model learned  
NB_mlr$learner.model

#Predict on the dataset without passing the target feature
predictions_mlr = as.data.frame(predict(NB_mlr, newdata = data[,1:3]))

##Confusion matrix to check accuracy
table(predictions_mlr[,1],train$Survived)