source('testFunctions.R')

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
biOrigin <- matrix(NA, nrow = A, ncol = B)
biRanked <- matrix(NA, nrow = A, ncol = B)
for (i in 1 : A){
  for (j in 1 : B){
    data <- normalMatrix(M, N, m, n, theta[i])
    rankeddata <- matrix(rank(data), M, N)
    biOrigin[i, j] <- biPermuTest(data, m, n)$pvalue
    biRanked[i, j] <- uniPermuTest(rowrankeddata, m, n)$pvalue
  }
}

save(biOrigin, biRanked, file = 'biRank.Rdata')