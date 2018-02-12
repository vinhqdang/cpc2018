CPC15_isStochasticDom  = function( DistA, DistB ) {
#Check if one distribution dominates stochastically the other
#   Input: 2 discrete distributions which are set as matrices of 1st column
#   as outcome and 2nd its probability. Output: 'is' a logical output,
#   'which' a char output ('A', 'B', NaN)

na= nrow(DistA)
nb= nrow(DistB)
dom =FALSE
if (identical(DistA,DistB)) {
  dom = FALSE
  which = NaN}
else {
  tempa = matrix(1,nrow=na,ncol=1)
  tempb = matrix(1,nrow=nb,ncol=1)
  for (i in 1:nb) {
    sumpa = 0#DistA(i,2)
    j = 1;
    sumpb =sum(DistB[1:i,2])
    while ((sumpa != 1) && (j<=na) && (sumpa + DistA[j,2]  <= sumpb)){
      sumpa = sumpa + DistA[j,2]
      if (sumpa == sumpb) {break}
      j = j +1
    }
    if (j > na) {j = na}
    if (DistB[i,1] < DistA[j,1]){  
      tempb[i] = 0;
      break
    } 
  }
  if (all(tempb!=0)){
    dom = TRUE
    which = 'B'}
  else {
    for (i in 1 : na) {
      sumpb = 0#DistA(i,2)
      j = 1
      sumpa =sum(DistA[1:i,2])
      while ((sumpb != 1) && (j<= nb) && (sumpb + DistB[j,2]  <= sumpa)){
        sumpb = sumpb + DistB[j,2]
        if (sumpa == sumpb) {break}
        j = j +1
      }
      if (j > nb ) {j = nb}
      if (DistA[i,1] < DistB[j,1]){
        tempa[i] = 0
        break
      }
    }
    if (all(tempa!=0)) {
      dom = TRUE
      which = 'A'}
    else {
      dom = FALSE
      which = NA
    }
  }
}
return(list("dom" = dom, "which" = which))
}


