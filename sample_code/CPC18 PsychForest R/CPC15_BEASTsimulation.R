CPC15_BEASTsimulation = function (DistA, DistB, Amb, Corr){
  # A single simulation run of the original BEAST model
  SIGMA = 7
  KAPA = 3
  BETA = 2.6
  GAMA = 0.5
  PSI = 0.07
  THETA = 1
  
  nTrials = 25
  firstFeedback = 6
  nBlocks = 5
  
  #draw personal traits
  sigma = SIGMA*runif(1)
  kapa = sample(KAPA,1)
  beta = BETA*runif(1)
  gama = GAMA*runif(1)
  psi = PSI*runif(1)
  theta = THETA*runif(1)
  
  pBias = rep(nTrials - firstFeedback+1,1)
  ObsPay = matrix(0,nrow=nTrials - firstFeedback+1,ncol=2) # observed outcomes in A (col1) and B (col2)
  Decision = matrix(NA,nrow=nTrials,ncol=1)
  simPred = matrix(NA,nrow=1,ncol=nBlocks)
  # Useful variables
  nA = nrow(DistA) # num outcomes in A
  nB = nrow(DistB) # num outcomes in B

  if (Amb == 1){
    ambiguous = TRUE}
  else{
    ambiguous = FALSE
  }

  nfeed = 0 # "t"; number of outcomes with feedback so far
  pBias[nfeed+1] = beta/(beta+1+nfeed^theta)
  MinA = DistA[1,1]
  MinB = DistB[1,1]
  MaxOutcome = max(DistA[nA,1],DistB[nB,1])
  SignMax = sign(MaxOutcome)
  if (MinA == MinB)
    {RatioMin = 1}
  else if (sign(MinA) == sign(MinB))
    {RatioMin = min(abs(MinA),abs(MinB))/max(abs(MinA),abs(MinB))}
  else
    {RatioMin = 0}
  
  Range = MaxOutcome - min(MinA, MinB)
  trivial = CPC15_isStochasticDom( DistA, DistB )
  BEVa = DistA[,1]%*%DistA[,2]
  if (ambiguous){
    UEVb = DistB[,1]%*%rep(1/nB,nB)
    BEVb = (1-psi)*(UEVb+BEVa)/2 + psi*MinB
    pEstB = rep(nB,1); # estimation of probabilties in Amb
    t_SPminb = (BEVb -mean(DistB[2:nB,1]))/(MinB-mean(DistB[2:nB,1]))
    if (t_SPminb < 0 )
      {pEstB[1] = 0}
    else if (t_SPminb > 1) 
      {pEstB[1] = 1}
    else
      {pEstB[1] = t_SPminb}
    
    pEstB[2:nB] = (1-pEstB[1])/(nB-1)
  }
  else {
    pEstB = DistB[,2]
    BEVb = DistB[,1]%*%pEstB
  }
  
  # simulation of decisions
  for (trial in 1:nTrials){
    STa = 0
    STb = 0
    # mental simulations
    for (s in 1:kapa) {
      rndNum = runif(2)
      if (rndNum[1] > pBias[nfeed+1]){ # Unbiased technique
        if (nfeed == 0) {
          outcomeA = distSample(DistA[,1],DistA[,2],rndNum[2])
          outcomeB = distSample(DistB[,1],pEstB,rndNum[2])}
        else {
          uniprobs = rep(1/nfeed,nfeed)
          outcomeA = distSample(ObsPay[1:nfeed,1],uniprobs,rndNum[2])
          outcomeB = distSample(ObsPay[1:nfeed,2],uniprobs,rndNum[2])
        }}
      else if (rndNum[1] > (2/3)*pBias[nfeed+1]){ #uniform
        outcomeA = distSample(DistA[,1],rep(1/nA,nA),rndNum[2])
        outcomeB = distSample(DistB[,1],rep(1/nB,nB),rndNum[2])}
      else if (rndNum[1] > (1/3)*pBias[nfeed+1]){ #contingent pessimism
        if (SignMax > 0 && RatioMin < gama){
          outcomeA = MinA
          outcomeB = MinB}
        else{
          outcomeA = distSample(DistA[,1],rep(1/nA,nA),rndNum[2])
          outcomeB = distSample(DistB[,1],rep(1/nB,nB),rndNum[2])
        }}
      else{ # Sign
        if (nfeed == 0){
          outcomeA = Range * distSample(sign(DistA[,1]),DistA[,2],rndNum[2])
          outcomeB = Range * distSample(sign(DistB[,1]),pEstB,rndNum[2])}
        else{
          uniprobs = rep(1/nfeed,nfeed)
          outcomeA = Range * distSample(sign(ObsPay[1:nfeed,1]),uniprobs,rndNum[2])
          outcomeB = Range * distSample(sign(ObsPay[1:nfeed,2]),uniprobs,rndNum[2])
        }
      }
      STa = STa + outcomeA
      STb = STb + outcomeB
    }
    STa = STa/kapa
    STb = STb/kapa
    
    #error term
    if (trivial$dom)
      {error = 0}
    else
      error = sigma*rnorm(1) # positive values contribute to attraction to A
    
    # decision
    Decision[trial] = (BEVa - BEVb) + (STa - STb) + error < 0
    if ((BEVa - BEVb) + (STa - STb) + error == 0)
      Decision[trial] = sample(2,1) -1
  
    if (trial >= firstFeedback){ # got feedback
      nfeed = nfeed +1
      pBias[nfeed+1] = beta/(beta+1+nfeed^theta)
      rndNumObs = runif(1)
      ObsPay[nfeed,1] = distSample(DistA[,1],DistA[,2],rndNumObs) # draw outcome from A
      if (Corr == 1)
        {ObsPay[nfeed,2] = distSample(DistB[,1],DistB[,2],rndNumObs)}
      else if (Corr == -1)
        {ObsPay[nfeed,2] = distSample(DistB[,1],DistB[,2],1-rndNumObs)}
      else
        {ObsPay[nfeed,2] = distSample(DistB[,1],DistB[,2],runif(1))} # draw outcome from B
      if (ambiguous)
        {BEVb = (1-1/(nTrials-firstFeedback+1))*BEVb + 1/(nTrials-firstFeedback+1)*ObsPay[nfeed,2]}
    }
  }
  
  #compute B-rates for this simulation
  blockSize = nTrials/nBlocks
  for (b in 1:nBlocks)
    {simPred[b] = mean(Decision[((b-1)*blockSize+1):(b*blockSize)])}
  return(simPred)
}