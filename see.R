library(nnet)
accesslogml_training <- read.csv("~/Rscripta/accesslogml_training.csv")
 
seeds = accesslogml_training 
seedstrain<- sample(1:4999,3000)
 seedstest <- setdiff(1:4999,seedstrain)
  ideal <- class.ind(seeds$result)
  seedsANN = nnet(seeds[seedstrain,-12], ideal[seedstrain,], size=10, softmax=TRUE)
   predict(seedsANN, seeds[seedstrain,-8], type="class")
    table(predict(seedsANN, seeds[seedstest,-8], type="class"),seeds[seedstest,]$result)