# setwd("---")
# rm(list = ls())

#############################################################################################################
### Section A: Please change this section to include your sourced files/data and load necessary packeages ###
#############################################################################################################
library("randomForest")
load("PF_TrainSetExample.RData")
source('distSample.R')
source('get_PF_Features.R')
source('CPC18_getDist.R')
source('CPC18_get_pBetter.R')
source('CPC18_PF_pred.R')
source('CPC15_isStochasticDom.R')
source('CPC15_BEASTsimulation.R')
source('CPC15_BEASTpred.R')
### End of Section A ###

####################################################
### Section B: Please do not change this section ###
####################################################
# load problems to predict (in this example, the estimation set problems)
Data = read.csv("CPC18_EstSet.csv", header = T)
# useful variables
nProblems = nrow(Data)
PredictedAll = matrix(NA,ncol=5,nrow=nProblems)
### End of Section B ###

#################################################################
### Section C: Please change only lines 47-53 in this section ###
#################################################################
for (prob in 1:nProblems) {
  #read problem's parameters
  Ha = Data$Ha[prob]
  pHa = Data$pHa[prob]
  La = Data$La[prob]
  LotShapeA = Data$LotShapeA[prob]
  LotNumA = Data$LotNumA[prob]
  Hb = Data$Hb[prob]
  pHb = Data$pHb[prob]
  Lb = Data$Lb[prob]
  LotShapeB = Data$LotShapeB[prob]
  LotNumB = Data$LotNumB[prob]
  Amb = Data$Amb[prob]
  Corr = Data$Corr[prob]
  
  # please plug in here your model that takes as input the 12 parameters
  # defined above and gives as output a vector size 5 named "Prediction" 
  # in which each cell is the predicted B-rate for one block of five trials
  # example:  
  Prediction = CPC18_PF_pred(TrainData, Ha, pHa, La, LotShapeA, LotNumA, 
                             Hb, pHb, Lb, LotShapeB, LotNumB, Amb, Corr)
  # end of example
  
  PredictedAll[prob,] = Prediction;
  print(prob) # for verbose progression
}
### End of Section C ###

####################################################
### Section D: Please do not change this section ###
####################################################
# compute MSE
ObservedAll = Data[,c("B.1","B.2","B.3","B.4","B.5")]
probMSEs = 100*sapply((PredictedAll - ObservedAll)^2,mean);
totalMSE = mean(probMSEs)
print(paste("MSE over the ",nProblems, " problems: ", totalMSE))
### End of Section D ###