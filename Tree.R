library(rpart)
dataad <- read.csv("~/Rscripta/accesslogml_trainingNBD.csv")
dataa <- na.omit(dataad)
# grow tree 
fit <- rpart(dataa$result~  dataa$uni +  dataa$Workstation + dataa$Dep + dataa$Exp   + dataa$filesize + dataa$filetype ,
             method="class", data=dataa)

printcp(fit) # display the results 
plotcp(fit) # visualize cross-validation results 
summary(fit) # detailed summary of splits
par(mfrow=c(1,1))
# plot tree 
plot(fit, uniform=TRUE, 
     main="Classification Tree for Kyphosis")
text(fit, use.n=TRUE, all=TRUE, cex=.8)
pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

# plot the pruned tree 
plot(pfit, uniform=TRUE, 
     main="Pruned Classification Tree for Access Result")
text(pfit, use.n=TRUE, all=TRUE, cex=.8)
pred = predict(pfit, type="class")
table(pred)
table(pred,dataa$result)
a<-confusionMatrix(pred,as.factor(dataa$result),mode = "prec_recall", positive="1")
a


