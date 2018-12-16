# This script outputs a neural net to compute the sine function
# in the interval [0,1]

library('neuralnet')

# returns a dataframe "randData"  containing pairds (x,sinx) of data sample
getRandData <- function(numDataPts){
  # generates numDataPts values for predictor under uniform 
  # distribution ranging between 0 and 1
  
  indVar <- runif(numDataPts, min = 0, max = 1)
  
  # sets dependent variable as sin of predictor
  depVar <- sin(indVar)
  
  # binds independent and dependent variables by columns
  randData <- cbind(indVar, depVar)
  
  return(randData)
}

# this function returns a sigmoid of x
sigmoid = function(x) {
  1 / (1 + exp(-x))
}

# this function returns a feedforward neural net by backward propagation training
getSigmoid <- function(trainingData){
  
  nn <- neuralnet(trainingData$result~trainingData$userid + trainingData$Salary + trainingData$uni + trainingData$Passingyear + trainingData$Workstation + trainingData$Dep + trainingData$Exp + trainingData$fileid + trainingData$creatorid + trainingData$filesize + trainingData$filetype, 
                  data = as.data.frame(trainingData), hidden = c(3, 2),
                  # two hidden layers with 3 neurons at first and 2 at second layers
                  linear.output = F,  stepmax = 4000, 
                  learningrate = 0.01)	# sigmoid as an activation function
  
# pdf("fnn.pdf") # creates a pdf with name fnn.pdf
  plot(nn) # plots neural network
# dev.off()	# saves the plot in the file and closes it
  
  return(nn) # returns the neural network model
}

# this function returns the mean square error for a trained nn model
getPerf <- function(testingData, model){
  
  cols <- ncol(testingData) - 1
  predict <- 0
  results <- 0
  
  # uses the model for prediction on testing dataset		
  predict <- compute(model, testingData[,c("userid",   
                                            "Salary", "uni", "Passingyear", "Workstation",   
                                            "Dep","Exp","fileid","creatorid","filesize","filetype")
                                         ])
  
  # scales the results of prediction for mean squared error (mse) calculation
  results <- predict$net.result*(max(testingData$result)-
                                   min(testingData$result))+min(testingData$result)
  
  # scales the dependent variables in test dataset for mse calculation
  test <- as.data.frame(testingData$result *
                          (max(testingData$result)-min(testingData$result))+min(testingData$result))
# plot(results)
  #plot(test)
  mse <- sum((test - results)^2)/nrow(test)
 # confusionMatrix(as.factor(abs(round(test))),as.factor(testingData$result))
  return(mse)
}

main <- function(){
  
  # generate 32 datapoints randomly 
  dataFrame <- read.csv("~/Rscripta/accesslogml_trainingNBD.csv")
  
  row <- nrow(dataFrame)
  ind <- sample(1:row, as.integer(0.7 * row))
  
  # select training and testing datasets randomly
  trainingData <- dataFrame[ind, ]
  testingData <- dataFrame[-ind, ]
  
  # gets the model
  sig <- getSigmoid(trainingData)
  
  # calculate the performance
  perfFnn <- getPerf(testingData, sig)
  
  print(paste("The mean squared error of the neural networks based on sigmoid activation is",
              perfFnn, sep = " "))
  
}
main()
