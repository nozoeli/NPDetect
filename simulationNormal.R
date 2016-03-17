source('testFunctions.R')
#------------initialization-------------------
M <- 100
N <- 120
m <- 30
n <- 25
crit <- sqrt(2*(m*log(M/m) + n*log(N/n))/m/n)
mu <- ((1:8) * 0.125 + 0.5) * crit
#------------iterations-----------------------
A <- length(mu)
B <- 50
pstat <- matrix(NA, nrow = A, ncol = B)
rowstat <- matrix(NA, nrow = A, ncol = B)
mcstat <- matrix(NA, nrow = A, ncol = B)
for (i in 1 : A){
  for (j in 1 : B){
    data <- normalMatrix(M, N, m, n, mu[i])
    pstat[i, j] <- biPermuTest(data, m, n)$pvalue
    rowstat[i, j] <- uniPermuTest(data, m, n)$pvalue
    mcstat[i, j] <- testMonteCarlo(data, m, n)$pvalue
  }
}
write(pstat, file = 'output_permu_normal.txt', ncolumns = A)
write(rowstat, file = 'output_row_normal.txt', ncolumns = A)
write(mcstat, file = 'output_mc_normal.txt', ncolumns = A)