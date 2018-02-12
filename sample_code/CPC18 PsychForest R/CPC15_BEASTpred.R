CPC15_BEASTpred = function( Ha, pHa, La, LotShapeA, LotNumA, Hb, pHb, Lb, LotShapeB, LotNumB, Amb, Corr ) {
  # Prediction of (the original) BEAST model for one problem
  
  Prediction = rep(0,5)
  
  # get both options' distributions
  DistA = CPC18_getDist(Ha, pHa, La, LotShapeA, LotNumA)
  DistB = CPC18_getDist(Hb, pHb, Lb, LotShapeB, LotNumB)
    
  # run model simulation nSims times
  nSims = 4000;
  for (sim in 1:nSims){
    simPred = CPC15_BEASTsimulation(DistA, DistB, Amb, Corr);    
    Prediction = Prediction + (1/nSims)*simPred;
  }
  return(Prediction)
}
    