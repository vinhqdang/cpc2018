# setwd("---")
# rm(list = ls())

################################################################################
### Section A: Please change this section to include your sourced files/data ###
################################################################################
source('CPC18_BEASTsd_pred.R')
source('CPC18_BEASTsd_simulation.R')
source('distSample.R')
source('CPC18_getDist.R')
source('CPC18_get_pBetter.R')
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
### Section C: Please change only lines 42-47 in this section ###
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
  Prediction = CPC18_BEASTsd_pred(Ha, pHa, La, LotShapeA, LotNumA, Hb, pHb, Lb, LotShapeB, LotNumB, Amb, Corr)
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
