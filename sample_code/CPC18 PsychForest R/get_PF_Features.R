get_PF_Features = function(Ha, pHa, La, LotShapeA, LotNumA, Hb, pHb, Lb, LotShapeB, LotNumB, Amb, Corr){
  #Finds the values of the engineered features that are part of Psychological Forest
  # Gets as input the parameters defining the choice problem in CPC18 and returns 
  # as output a dataframe with this problem's features
  
  # To compute the distribution's standard deviation
  getSD <- function(vals,probs){
    vals = as.numeric(na.omit(vals))
    probs = as.numeric(na.omit(probs))
    m = vals %*% probs
    sqds= (vals-m[1])^2
    var = probs %*% sqds
    return(sqrt(var))
  }
  
  # Compute "naive" and "psychological" features as per Plonsky, Erev, Hazan, and Tennenholtz, 2017
  DistA = CPC18_getDist(Ha, pHa, La, LotShapeA, LotNumA)
  DistB = CPC18_getDist(Hb, pHb, Lb, LotShapeB, LotNumB)
  diffEV = ((DistB[,1]%*%DistB[,2]) - (DistA[,1]%*%DistA[,2]))[1]
  diffSDs = (getSD(DistB[,1],DistB[,2]) - getSD(DistA[,1],DistA[,2]))[1]
  MinA = DistA[1,1]
  MinB = DistB[1,1]
  diffMins = MinB - MinA
  nA = nrow(DistA)
  nB = nrow(DistB)
  MaxA = DistA[nA,1]
  MaxB = DistB[nB,1]
  diffMaxs = MaxB - MaxA
  
  diffUV = ((DistB[,1] %*% (rep(1,nB)/nB)) - (DistA[,1] %*% (rep(1,nA)/nA)))[1]
  if (Amb == 1) {ambiguous = TRUE} else {ambiguous = FALSE}
  MaxOutcome = max(MaxA,MaxB)
  SignMax = sign(MaxOutcome)
  if (MinA == MinB) {
    RatioMin = 1} 
  else if (sign(MinA) == sign(MinB)) {
    RatioMin = min(abs(MinA),abs(MinB))/max(abs(MinA),abs(MinB))} 
  else {
    RatioMin = 0
  }
  Range = MaxOutcome - min(MinA, MinB)
  diffSignEV = ((Range*(sign(DistB[,1]))%*%DistB[,2]) - (Range*(sign(DistA[,1]))%*%DistA[,2]))[1]
  trivial = CPC15_isStochasticDom( DistA, DistB )
  whchdom = trivial[[2]]
  Dom = 0
  if ((trivial$dom)&(whchdom == "A")) {Dom = -1}
  if ((trivial$dom)&(whchdom == "B")) {Dom = 1}
  BEVa = DistA[,1]%*%DistA[,2]
  if (ambiguous){
    UEVb = DistB[,1]%*%rep(1/nB,nB)
    BEVb = (UEVb+BEVa+MinB)/3 
    pEstB = rep(nB,1) # estimation of probabilties in Amb
    t_SPminb = (BEVb -mean(DistB[2:nB,1]))/(MinB-mean(DistB[2:nB,1]));
    if (t_SPminb < 0 ) {pEstB[1] = 0} else if (t_SPminb > 1) {pEstB[1] = 1} else {pEstB[1] = t_SPminb}
    pEstB[2:nB] = (1-pEstB[1])/(nB-1)}
  else {
    pEstB = DistB[,2]
    BEVb = DistB[,1]%*%pEstB;
  }
  diffBEV0 = (BEVb - BEVa)[1]
  BEVfb = (BEVb+(DistB[,1]%*%DistB[,2]))/2
  diffBEVfb = (BEVfb - BEVa)[1]
  
  sampleDistB = cbind(DistB[,1],pEstB)
  probsBetter = get_pBetter(DistA,sampleDistB,corr=1)
  pAbetter = probsBetter[[1]]
  pBbetter = probsBetter[[2]]
  pBbet_Unbiased1 = pBbetter - pAbetter
  
  sampleUniDistA = cbind(DistA[,1],rep(1/nA,nA))
  sampleUniDistB = cbind(DistB[,1],rep(1/nB,nB))
  probsBetterUni = get_pBetter(sampleUniDistA,sampleUniDistB,corr=1)
  pBbet_Uniform = probsBetterUni[[2]] - probsBetterUni[[1]]
  
  sampleSignA = DistA
  sampleSignA[,1] = sign(sampleSignA[,1])
  sampleSignB = cbind(sign(DistB[,1]),pEstB)
  probsBetterSign = get_pBetter(sampleSignA,sampleSignB,corr=1)
  pBbet_Sign1 = probsBetterSign[[2]] - probsBetterSign[[1]]
  sampleSignBFB = cbind(sign(DistB[,1]),DistB[,2])
  if (Corr == 1){
    probsBetter = get_pBetter(DistA,DistB,corr=1)
    probsBetterSign = get_pBetter(sampleSignA,sampleSignBFB,corr=1)}
  else if (Corr == -1){
    probsBetter = get_pBetter(DistA,DistB,corr=-1)
    probsBetterSign = get_pBetter(sampleSignA,sampleSignBFB,corr=-1)}
  else{
    probsBetter = get_pBetter(DistA,DistB,corr=0)
    probsBetterSign = get_pBetter(sampleSignA,sampleSignBFB,corr=0)
  }
  pBbet_UnbiasedFB = probsBetter[[2]] - probsBetter[[1]]
  pBbet_SignFB = probsBetterSign[[2]] - probsBetterSign[[1]]
  
  # create features data frame
  tmpFeats = data.frame(Ha, pHa, La, LotShapeA, LotNumA, Hb, pHb, Lb, LotShapeB, LotNumB, Amb, Corr,
                        diffEV, diffSDs, diffMins, diffMaxs, diffUV, RatioMin, SignMax, pBbet_Unbiased1,
                        pBbet_UnbiasedFB, pBbet_Uniform, pBbet_Sign1, pBbet_SignFB, Dom, diffBEV0, 
                        diffBEVfb, diffSignEV)
  # duplicate features data frame as per number of blocks
  Feats = tmpFeats[rep(seq(nrow(tmpFeats)),5),]
  
  # get BEAST model prediction as feature
  beastPs = CPC15_BEASTpred(Ha, pHa, La, LotShapeA, LotNumA, Hb, pHb, Lb, LotShapeB, LotNumB, Amb, Corr)
  Feats$BEASTpred = t(beastPs)
  
  Feats$block = c(1:5)
  Feats$Feedback = 1
  Feats$Feedback[Feats$block==1] = 0
  
  return(Feats)
}
