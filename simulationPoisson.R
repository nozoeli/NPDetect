source('testFunctions.R')
#------------initialization-------------------
M <- 130
N <- 110
m <- 40
n <- 20
crit <- sqrt(2*(m*log(M/m) + n*log(N/n))/m/n)
mu <- exp(((1:8) * 0.125 + 0.5) * crit)
#------------iterations-----------------------
A <- length(mu)
B <- 50
mcstat <- matrix(NA, nrow = A, ncol = B)
pstat <- matrix(NA, nrow = A, ncol = B)
for (i in 1 : A){
for (j in 1 : B){
  data <- poissonMatrix(M, N, m, n, mu[i])
  mcstat[i, j] <- testMonteCarloPoi(data, m, n)$pvalue
  pstat[i,j] <- biPermuTest(data, m, n)$pvalue
}
}
write(pstat, file = 'output_poisson_permu.txt', ncolumns = A)
write(mcstat, file = 'output_poisson_mc.txt', ncolumns = A)