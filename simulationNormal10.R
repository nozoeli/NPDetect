source('testFunctions.R')
#------------initialization-------------------
M <- 200
N <- 100
m <- 10
n <- 15
crit <- sqrt(2*(m*log(M/m) + n*log(N/n))/m/n)
theta <- ((1:8) * 0.125 + 0.5) * crit
#------------iterations-----------------------
A <- length(theta)
B <- 200
biPermuNormal10 <- matrix(NA, nrow = A, ncol = B)
uniPermuNormal10 <- matrix(NA, nrow = A, ncol = B)
MCNormal10 <- matrix(NA, nrow = A, ncol = B)
uniRanked10 <- matrix(NA, nrow = A, ncol = B)
biRanked10 <- matrix(NA, nrow = A, ncol = B)
for (i in 1 : A){
  for (j in 1 : B){
    data <- normalMatrix(M, N, m, n, theta[i])
    rankeddata <- matrix(rank(data), M, N)
    rowrankeddata <- rowrank(data)
    biPermuNormal10[i, j] <- biPermuTest(data, m, n)$pvalue
    uniPermuNormal10[i, j] <- uniPermuTest(data, m, n)$pvalue
    MCNormal10[i, j] <- testMonteCarlo(data, m, n)$pvalue
    biRanked10[i, j] <- biPermuTest(rankeddata, m, n)$pvalue
    uniRanked10[i, j] <- uniPermuTest(rowrankeddata, m, n)$pvalue
  }
}

save(biPermuNormal10, uniPermuNormal10, MCNormal10, uniRanked10, biRanked10, file = 'Normal10.Rdata')
