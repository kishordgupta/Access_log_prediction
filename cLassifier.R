# This script trains a neural nets classifier for flower species based on 
# morphological features such as sepal/petal width and length,
# as given by the standrad data= set "iris"  dowlnloaded from
# "http://www.heatonresearch.com/dload/data/iris.csv"

# this library is being used for setting the labels for the data
library(nnet)

# this library is used to train a perceptron
library(neuralnet)

# reads the Data
irisData <- read.csv("~/Rscripta/accesslogml_trainingNBD.csv", header = T, sep = ",")
dataa<-irisData
# Alternatively, you can use this command to get it from 
# 'irisdata <- read.csv("http://www.heatonresearch.com/dload/data/iris.csv",head=TRUE,sep=",")'

# selects 100 indices (about 67%) for records from the 150 in the dataset
trainDataInds <- sample(1:2250,1150, replace = FALSE)


# pulls the corresponding 100 full data points 
trainData <- irisData[trainDataInds, ]

# sets the species features to 0 except for the one occurring first (setosa), which is set to 1
# each entry here is a 0 or a 1 
setosas <- class.ind(irisData$result)

nn <- neuralnet(dataa$result~dataa$Salary +dataa$Workstation + dataa$Dep + dataa$Exp   + dataa$filesize + dataa$filetype, 
                # replaces the last feature by the corresponding lables in the target feature
                data = as.data.frame(cbind(trainData[, -13], setosas[trainDataInds])),
                hidden = c(3,2), linear.output = F)
# hidden = 0 is for no hidden layers
# IN another case, the hidden = c(3,2) would mean two hidden layers [3 2], so the NN arch is [4 3 2 1]

# show me the goodie nn (next slide)	
plot(nn)

# selects the test dataset as the complementary points of the training set
testData <- irisData[-trainDataInds, ]

# get the list of predictions (100 real number outputs between 0 and 1) for the test dataset
nnoutputs <- compute(nn, testData[,c("Salary","Workstation",   
                                     "Dep","Exp","filesize","filetype")
                                  ])$net.result

# define a function to return a list containing rounded results
roundResults <- function(predictions) {
  
  roundedPreds <- list()
  
  # rounds each prediction to 0 if it is less than 0.5 else 1
  for(i in 1:length(predictions)){
    roundedPreds[i] <- round(predictions[i], digits = 0)
  } 
  
  roundedPreds <- unlist(roundedPreds)
  
  return(roundedPreds)
} #  end "predictions" def'n

# collapse analog output to a category (1=setosa, 0=non) 
roundedPreds <- roundResults(nnoutputs)

predictions <- list()


predictions <- unlist(predictions)

# create a dataframe adding true labels of each data point
predictedVsTrue <- data.frame(predictions, testData[, 13])
b<-confusionMatrix(as.factor(roundedPreds),as.factor(dataa$result),mode = "prec_recall", positive="1")
b
# Now assess the modle by comparing predictions with the true value (accuracy)
accuracy <- 0
for(i in 1:nrow(predictedVsTrue)){
  if(predictedVsTrue[i, 1] == predictedVsTrue[i, 2]){
    accuracy <- accuracy + 1
  } else if(!(predictedVsTrue[i, 2] == "1") & !(predictedVsTrue[i, 1] == "1")){
    accuracy <- accuracy + 1
  }
}

# Finally, output the results so people can understand the output without having to see this code
accuracy <- accuracy/nrow(predictedVsTrue) * 100
print(paste("The accuracy of the model is :", accuracy, "%", sep = " "))

