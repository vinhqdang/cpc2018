# setwd("---")
# rm(list = ls())

###########################################################################
### Section A: Please change this section to include your sourced files ###
###########################################################################
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
Data_FullDist = read.csv("CPC18_EstSet_fullDist.csv", header = T)
# useful variables
nProblems = nrow(Data_FullDist)
PredictedAll = matrix(NA,ncol=5,nrow=nProblems)
### End of Section B ###
    
#################################################################
### Section C: Please change only lines 47-62 in this section ###
#################################################################
for (prob in 1:nProblems) {
  Amb = Data_FullDist$Amb[prob];
  Corr = Data_FullDist$Corr[prob];
  DistA=c()
  DistB=c()
  t1=as.numeric(Data_FullDist[prob,c('av1','ap1','av2','ap2','av3','ap3','av4','ap4','av5','ap5',
                            'av6','ap6','av7','ap7','av8','ap8','av9','ap9','av10','ap10')])
  for (i in 1:10){
    if (!(is.na(t1[i*2]))){
      DistA = rbind(DistA,c(t1[2*i-1],t1[2*i]))
    }
  } 
  t2 = as.numeric(Data_FullDist[prob,c('bv1','bp1','bv2','bp2','bv3','bp3','bv4','bp4','bv5','bp5',
                              'bv6','bp6','bv7','bp7','bv8','bp8','bv9','bp9','bv10','bp10')])
  for (i in 1:10){
    if (!(is.na(t2[i*2]))){
      DistB = rbind(DistB,c(t2[2*i-1],t2[2*i]))
    }
  }
  
  # please plug in here your model that takes as input DistA, the 
  # distribution of Option A, DistB, the distribution of Option B,
  # and the parameters Amb and Corr. The distributions are in 
  # matrix-list form (each row is an outcome and its probability). 
  # The rows are sorted in ascending order of outcomes. 
  # The model's output should be a vector size 5 named "Prediction" 
  # in which each cell is the predicted B-rate for one block of five trials
# example: 
  Prediction = rep(0,5)
  probsBetter = get_pBetter(DistA,DistB,corr=1, accuracy = 100000)
  nSims = 10000
  for (sim in 1:nSims){
    simPred = CPC18_BEASTsd_simulation(DistA, DistB, Amb, Corr, probsBetter)
    Prediction = Prediction + (1/nSims)*simPred
  }
# end of example
  
  PredictedAll[prob,] = Prediction;    
}
### End of Section C ###

####################################################
### Section D: Please do not change this section ###
####################################################
# compute MSE
probMSEs = 100*sapply((PredictedAll - ObservedAll)^2,mean);
totalMSE = mean(probMSEs)
print(paste("MSE over the ",nProblems, " problems: ", totalMSE))
### End of Section D ###