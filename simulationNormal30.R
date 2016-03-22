source('testFunctions.R')
#------------initialization-------------------
M <- 200
N <- 100
m <- 30
n <- 10
crit <- sqrt(2*(m*log(M/m) + n*log(N/n))/m/n)
theta <- ((1:8) * 0.125 + 0.5) * crit
#------------iterations-----------------------
A <- length(theta)
B <- 200
biPermuNormal30 <- matrix(NA, nrow = A, ncol = B)
uniPermuNormal30 <- matrix(NA, nrow = A, ncol = B)
MCNormal30 <- matrix(NA, nrow = A, ncol = B)
uniRanked30 <- matrix(NA, nrow = A, ncol = B)
biRanked30 <- matrix(NA, nrow = A, ncol = B)
for (i in 1 : A){
  for (j in 1 : B){
    data <- normalMatrix(M, N, m, n, theta[i])
    rankeddata <- matrix(rank(data), M, N)
    rowrankeddata <- rowrank(data)
    biPermuNormal30[i, j] <- biPermuTest(data, m, n)$pvalue
    uniPermuNormal30[i, j] <- uniPermuTest(data, m, n)$pvalue
    MCNormal30[i, j] <- testMonteCarlo(data, m, n)$pvalue
    biRanked30[i, j] <- biPermuTest(rankeddata, m, n)$pvalue
    uniRanked30[i, j] <- uniPermuTest(rowrankeddata, m, n)$pvalue
  }
}

save(biPermuNormal30, uniPermuNormal30, MCNormal30, uniRanked30, biRanked30, file = 'Normal30.Rdata')
