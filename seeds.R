library(nnet)
accesslogml_training <- na.omit(read.csv("~/Rscripta/accesslogml_trainingNBD.csv")

seeds = accesslogml_training 
seedstrain<- sample(1:2156,2000)
seedstest <- setdiff(1:2156,seedstrain)
ideal <- class.ind(seeds$result)
seedsANN = nnet(seeds[seedstrain,-13], ideal[seedstrain,], size=20, softmax=TRUE)
a=predict(seedsANN, seeds[seedstrain,-13], type="class")
table(predict(seedsANN, seeds[seedstest,-13], type="class"),seeds[seedstest,]$result)

plot(seedsANN)
