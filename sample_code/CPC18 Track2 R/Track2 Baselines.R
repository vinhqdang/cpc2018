# setwd("---")
# rm(list = ls())

# A function used to evaluate predictive performance
my_evaluate <- function(y,y_hat,do.MSE = TRUE, do.cor = TRUE, do.plot = TRUE){
  rmse = NA
  cor_yy = NA
  if (do.MSE){
    rmse = sqrt(mean((y-y_hat)^2))
    print(paste("RMSE is ", rmse))
  }
  if (do.cor){
    cor_yy = cor(y,y_hat)
    print(paste("correlation is ", cor_yy))
  }
  if (do.plot){
    plot(y_hat,y)
    lmline = lm(y~y_hat)
    abline(a=lmline$coefficients[1],b=lmline$coefficients[2])
    abline(a=0,b=1,col = "red")
  }
  return(list(rmse,cor_yy))
}

# Read data
individualBlockAvgs = read.csv("individualBlockAvgs.csv", header = T)

# Transform outputs to be consistent with maximization (i.e., not minimization)
pBpMaxTransform <- function(origVec, isBMax){
  newVec = origVec
  newVec[!isBMax] = 1-origVec[!isBMax]
  return(newVec)
}
isBMax = individualBlockAvgs$diffEV >= 0
individualBlockAvgs$B = pBpMaxTransform(individualBlockAvgs$B, isBMax)

# keep only relevant data
data = individualBlockAvgs[,c("SubjID","GameID","block","B")]
data$isBMax = isBMax

###### split data to train and test ########
source("splitLongData.R")
data$SubjID = as.numeric(as.character(data$SubjID))
newData = data[data$SubjID >= 60000,]
dataSets = splitLongData(newData,seed=1)
trainD = data.frame(dataSets[1])
trainD$SubjID = as.numeric(as.character(trainD$SubjID))
trainD = rbind(trainD,data[data$SubjID<60000,])
testD = data.frame(dataSets[2])
trainD$SubjID = factor(trainD$SubjID)
testD$SubjID = factor(testD$SubjID, levels = levels(trainD$SubjID))

#####################################
######### Naive baseline: avgs ######
#####################################
avgBinTrain = aggregate(B~GameID+block,data= trainD, FUN = mean)
testWithPreds = merge(testD,avgBinTrain,by=c("GameID","block"))
my_evaluate(pBpMaxTransform(testWithPreds$B.x, testWithPreds$isBMax),pBpMaxTransform(testWithPreds$B.y, testWithPreds$isBMax))

######################################
######### Factorization machine ######
######################################
library(FactoRizationMachines)
# Treating different blocks as different games
trainD$new.GameID = paste(trainD$GameID,trainD$block, sep = ".")
trainD$new.GameID = factor(trainD$new.GameID)
testD$new.GameID = paste(testD$GameID,testD$block, sep = ".")
testD$new.GameID = factor(testD$new.GameID, levels = levels(trainD$new.GameID))
# for convinience 
trainD$GameID = trainD$new.GameID
testD$GameID = testD$new.GameID
drops <- c("block","new.GameID")
trainD = trainD[ , !(names(trainD) %in% drops)]
testD = testD[ , !(names(testD) %in% drops)]
# prepare FM input
data.train = model.matrix(B~., data= trainD, contrasts.arg = 
                            lapply(trainD[,sapply(trainD, is.factor) ], contrasts, contrasts=FALSE))[,-1]
target.train = trainD$B
data.test = model.matrix(B~., data= testD, contrasts.arg = 
                           lapply(testD[,sapply(testD, is.factor) ], contrasts, contrasts=FALSE))[,-1]
target.test = testD$B
model=FM.train(data.train,target.train,c(1,100),iter= 100, regular=c(0.001,0.05),intercept= TRUE)
predFM = predict(model,data.test)
testWithPreds = data.frame(testD,predFM)
my_evaluate(pBpMaxTransform(testWithPreds$B, testWithPreds$isBMax),pBpMaxTransform(testWithPreds$predFM, testWithPreds$isBMax))



