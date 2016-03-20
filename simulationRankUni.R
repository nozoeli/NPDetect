source('testFunctions.R')
#-----------A unidimensional ranking function-----------------

rowrank <- function(X){  #Rank the matrix by the row
  rankedX <- matrix(NA, nrow(X), ncol(X))
  for (i in 1 : nrow(X)){
    rankedX[i,] <- rank(X[i,])
  }
  return(rankedX)
}

#------------initialization-------------------
M <- 100
N <- 200
m <- 20
n <- 15
crit <- sqrt(2*(m*log(M/m) + n*log(N/n))/m/n)
theta <- ((1:8) * 0.125 + 0.5) * crit
#------------iterations-----------------------
A <- length(theta)
B <- 200
uniOrigin <- matrix(NA, nrow = A, ncol = B)
uniRanked <- matrix(NA, nrow = A, ncol = B)
for (i in 1 : A){
  for (j in 1 : B){
    data <- normalMatrix(M, N, m, n, theta[i])
    rowrankeddata <- rowrank(data)
    uniOrigin[i, j] <- uniPermuTest(data, m, n)$pvalue
    uniRanked[i, j] <- uniPermuTest(rowrankeddata, m, n)$pvalue
  }
}

save(uniOrigin, uniRanked, file = 'uniRank.Rdata')