CPC18_PF_pred = function(TrainData, Ha, pHa, La, LotShapeA, LotNumA, Hb, pHb, Lb, LotShapeB, LotNumB, Amb, Corr ){
  # Prediction of Psychological Forest for one problem
  #
  #  This function gets as input 12 parameters which define a problem in CPC18
  #  and outputs Psych. Forest's prediction in that problem for five blocks of 
  #  five trials each (the first is without and the others are with feedback
  
  # get data frame of engineedred features's values for the prediction problem
  Feats = get_PF_Features(Ha, pHa, La, LotShapeA, LotNumA, Hb, pHb, Lb, LotShapeB, LotNumB, Amb, Corr)
  
  # make sure the features are in the same space as in the original train data
  Feats$LotShapeA = factor(Feats$LotShapeA, levels = levels(TrainData$LotShapeA))
  Feats$LotShapeB = factor(Feats$LotShapeB, levels = levels(TrainData$LotShapeB))
  Feats$Amb = factor(Feats$Amb, levels = levels(TrainData$Amb))
  Feats$Corr = factor(Feats$Corr, levels = levels(TrainData$Corr))
  Feats$SignMax = factor(Feats$SignMax, levels = levels(TrainData$SignMax))
  Feats$Dom = factor(Feats$Dom, levels = levels(TrainData$Dom))
  Feats$Feedback = factor(Feats$Feedback, levels = levels(TrainData$Feedback))
  
  nRuns = 20
  Prediction = rep(0,5)
  for (run in 1:nRuns){
    # train a random forest algorithm using all supplied features of the train data
    xtrain = TrainData[,2:32] 
    ytrain = TrainData[,33]
    rf.model = randomForest(x=xtrain,y=ytrain,keep.forest=T)
    
    # let the trained RF predict the prediction prbolem
    pred = predict(rf.model,Feats)
    Prediction = Prediction + (1/nRuns)*pred
  }
  return(Prediction)
}