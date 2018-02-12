splitLongData <- function(dd, seed = 1, nSubjTest = 30, nGamesPerSubjTest = 5){
  
  if ((!("SubjID" %in% colnames(dd))) | (!("GameID" %in% colnames(dd)))){
    print("data must include SubjID and GameID columns")
    return(NULL)
  }
  dd = dd[order(dd$SubjID,dd$GameID),]
  set.seed(seed)
  if (is.factor(dd$SubjID)){
    dd$SubjID = as.numeric(as.character(dd$SubjID))
  }
  if (is.factor(dd$GameID)){
    dd$GameID = as.numeric(as.character(dd$GameID))
  }
  subjs = unique(dd$SubjID)
  subj2remove = sample(subjs,nSubjTest)
  test = dd[1,]
  test = test[-1,]
  train = dd
  for (s in 1:nSubjTest){
    subjD = dd[dd$SubjID == subj2remove[s],]
    games = unique(subjD$GameID)
    games2remove = sample(games,nGamesPerSubjTest)
    for (g in 1:nGamesPerSubjTest){
      test = rbind(test,dd[(dd$SubjID == subj2remove[s]) & (dd$GameID == games2remove[g]),])
      train = train[-which((train$SubjID == subj2remove[s]) & (train$GameID == games2remove[g])),]
    }
  }

  dd$SubjID = factor(dd$SubjID)

  train$SubjID = factor(train$SubjID, levels = levels(dd$SubjID))
  test$SubjID = factor(test$SubjID, levels = levels(dd$SubjID))

  return(list(train,test))
}