
accesslogml_training <- read.csv("~/Rscripta/accesslogml_training.csv")
View(accesslogml_training)
 
data = accesslogml_training 
samplesize = 0.60 * nrow(data)
  set.seed(80)
 index = sample( seq_len ( nrow ( data ) ), size = samplesize )
 

      datatrain = data[ index, ]
     datatest = data[ -index, ]
      max = apply(data , 2 , max)
      min = apply(data, 2 , min)
      scaled = as.data.frame(scale(data, center = min, scale = max - min))
 library(neuralnet)

 trainNN = scaled[index , ]
 testNN = scaled[-index , ]
 
 
 NN = neuralnet(data$result ~ data$userid + data$Salary + data$uni + data$Passingyear + data$Workstation + data$Dep + data$Exp + data$fileid + data$creatorid + data$filesize + data$filetype , trainNN, hidden =c( 5, 3) , linear.output = T )
 plot(NN)
 summary(NN)
 
predict_testNN = (predict_testNN$net.result * (max(data$result) - min(data$result))) + min(data$result)

plot(datatest$result, predict_testNN, col='blue', pch=16, ylab = "predicted result NN", xlab = "real result")

abline(0,1)

# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((datatest$result - predict_testNN)^2) / nrow(datatest)) ^ 0.5

