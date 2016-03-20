source('testFunctions.R')
#------------initialization-------------------
M <- 100
N <- 200
m <- 10
n <- 15
crit <- sqrt(2*(m*log(M/m) + n*log(N/n))/m/n)
theta <- ((1:8) * 0.125 + 0.5) * crit
#------------iterations-----------------------
A <- length(theta)
B <- 200
MCPoi10 <- matrix(NA, nrow = A, ncol = B)
biPermuPoi10 <- matrix(NA, nrow = A, ncol = B)
for (i in 1 : A){
for (j in 1 : B){
  data <- poissonMatrix(M, N, m, n, theta[i])
  MCPoi10[i, j] <- testMonteCarloPoi(data, m, n)$pvalue
  biPermuPoi10[i,j] <- biPermuTest(data, m, n)$pvalue
}
}

save(biPermuPoi10, MCPoi10, file = 'Poi10.Rdata')