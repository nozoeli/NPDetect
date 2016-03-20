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
MCPoi20 <- matrix(NA, nrow = A, ncol = B)
biPermuPoi20 <- matrix(NA, nrow = A, ncol = B)
for (i in 1 : A){
for (j in 1 : B){
  data <- poissonMatrix(M, N, m, n, theta[i])
  MCPoi20[i, j] <- testMonteCarloPoi(data, m, n)$pvalue
  biPermuPoi20[i,j] <- biPermuTest(data, m, n)$pvalue
}
}

save(biPermuPoi20, MCPoi20, file = 'Poi20.Rdata')