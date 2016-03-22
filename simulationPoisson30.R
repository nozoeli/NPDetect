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
MCPoi30 <- matrix(NA, nrow = A, ncol = B)
biPermuPoi30 <- matrix(NA, nrow = A, ncol = B)
uniPermuPoi30 <- matrix(NA, nrow = A, ncol = B)
for (i in 1 : A){
for (j in 1 : B){
  data <- poissonMatrix(M, N, m, n, theta[i])
  MCPoi30[i, j] <- testMonteCarloPoi(data, m, n)$pvalue
  biPermuPoi30[i,j] <- biPermuTest(data, m, n)$pvalue
  uniPermuPoi30[i, j] <- uniPermuTest(data, m, n)$pvalue
}
}

save(biPermuPoi30, MCPoi30, uniPermuPoi30, file = 'Poi30.Rdata')