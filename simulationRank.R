source('testFunctions.R')
#-----------A unidimensional ranking function-----------------

rowrank <- function(A){  #Rank the matrix by the row
  rankedA <- matrix(NA, nrow(A), ncol(A))
  for (i in 1 : nrow(A)){
    rankedA[i,] <- rank(A[i,])
  }
  return(rankedA)
}

#------------initialization(same as before)-------------------
M <- 100
N <- 120
m <- 30
n <- 25
crit <- sqrt(2*(m*log(M/m) + n*log(N/n))/m/n)
mu <- ((1:8) * 0.125 + 0.75) * crit
#------------iterations-----------------------
A <- length(mu)
B <- 30
unipstat <- matrix(NA, nrow = A, ncol = B)
bipstat <- matrix(NA, nrow = A, ncol = B)
ranktest <- matrix(NA, nrow = A, ncol = B)
rowranktest <- matrix(NA, nrow = A, ncol = B)
for (i in 1 : A){
  for (j in 1 : B){
    data <- normalMatrix(M, N, m, n, mu[i])
    rowrankeddata <- rowrank(data)
    rankeddata <- matrix(rank(data), M, N)
    unipstat[i, j] <- uniPermuTest(data, m, n)$pvalue
    bipstat[i, j] <- biPermuTest(data, m, n)$pvalue
    ranktest[i, j] <- biPermuTest(rankeddata, m, n)$pvalue
    rowranktest[i, j] <- RowPermuTest(rowrankeddata, m, n)$pvalue
  }
}
write(unipstat, file = 'output_rank_unipermu.txt', ncolumns = A)
write(bipstat, file = 'output_rank_bipermu.txt', ncolumns = A)
write(ranktest, file = 'output_rank_ranktest.txt', ncolumns = A)
write(rowranktest, file = 'output_rank_rowranktest.txt', ncolumns = A)