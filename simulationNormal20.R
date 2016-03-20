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
biPermuNormal20 <- matrix(NA, nrow = A, ncol = B)
uniPermuNormal20 <- matrix(NA, nrow = A, ncol = B)
MCNormal20 <- matrix(NA, nrow = A, ncol = B)
for (i in 1 : A){
  for (j in 1 : B){
    data <- normalMatrix(M, N, m, n, theta[i])
    biPermuNormal20[i, j] <- biPermuTest(data, m, n)$pvalue
    uniPermuNormal20[i, j] <- uniPermuTest(data, m, n)$pvalue
    MCNormal20[i, j] <- testMonteCarlo(data, m, n)$pvalue
  }
}

save(biPermuNormal20, uniPermuNormal20, MCNormal20, file = 'Normal20.Rdata')
