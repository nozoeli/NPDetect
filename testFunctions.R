source('randomMatrix.R')
source('shabalin.R')

#-----------A unidimensional ranking function-----------------

rowrank <- function(X){  #Rank the matrix by the row
  rankedX <- matrix(NA, nrow(X), ncol(X))
  for (i in 1 : nrow(X)){
    rankedX[i,] <- rank(X[i,])
  }
  return(rankedX)
}

#----------------------Unidimensional Permutation Test----------
uniPermuTest <- function(X, m, n, alpha = 0.05, iter = 500){
  # The function returns the test result of scan test calibrated by unidimensional permutation
  # The test is calibrated by permutation, with 'iter' iterations, default 500
  # X : the matrix being tested
  # m, n : the size of the anomaly in the alternative hypothesis
  # alpha : the test level
  M <- nrow(X)
  N <- ncol(X)
  originSum <- largestSbmxShabalin(X, m, n)
  pvalue <- 0
  for (j in 1 : iter){
    permSum <- largestSbmxShabalin(t(apply(X,1,sample)), m, n)
    pvalue <- pvalue + (permSum >= originSum) / (iter + 1)
  }
  permTest = (pvalue <= alpha) + 0
  return(list('pvalue' = pvalue, 'test' = permTest))
}

#----------------------Bidimensional Permutation Test----------
biPermuTest <- function(X, m, n, alpha = 0.05, iter = 500){
  # The function returns a result of the test of an elevated m*n submatrix
  # The test is calibrated by permutation, with 'iter' iterations, default 500
  # A : the matrix being tested
  # m, n : the size of the anomaly in the alternative hypothesis
  # alpha : the test level
  M <- nrow(X)
  N <- ncol(X)
  originSum <- largestSbmxShabalin(X, m, n)
  vectorA <- as.vector(X)
  pvalue <- 0
  for (j in 1 : iter){
    permSum <- largestSbmxShabalin(matrix(sample(vectorA), M, N), m, n)
    pvalue <- pvalue + (permSum >= originSum) / (iter + 1)
  }
  permTest = (pvalue <= alpha) + 0
  return(list('pvalue' = pvalue, 'test' = permTest))
}

#------------------Test Calibrated by Monte Carlo (Normal)----------
testMonteCarlo <- function(X, m, n, alpha = 0.05, iter = 500){
  # The function provides the p-value calibrated by monte carlo method
  # The null distribution is N(0,1)
  # A : the matrix being tested
  # m, n : the size of the anomaly in the alternative hypothesis
  # alpha : the test level
  M <- nrow(X)
  N <- ncol(X)
  originSum <- largestSbmxShabalin(X, m, n)
  pvalue <- 0
  for (j in 1 : iter){
    mcSum <- largestSbmxShabalin(normalMatrix(M, N, 0, 0, 0), m, n)
    pvalue <- pvalue + (mcSum >= originSum) / (iter + 1)
  }
  mcTest = (pvalue <= alpha) + 0
  return(list('pvalue' = pvalue, 'test' = mcTest))
  
}

#------------------Test Calibrated by Monte Carlo (Poisson)----------

testMonteCarloPoi <- function(X, m, n, alpha = 0.05, iter = 500){
  # The function provides the p-value calibrated by monte carlo method
  # The null distribution is Poi(1) - 1
  # A : the matrix being tested
  # m, n : the size of the anomaly in the alternative hypothesis
  # alpha : the test level
  M <- nrow(X)
  N <- ncol(X)
  originSum <- largestSbmxShabalin(X, m, n)
  pvalue <- 0
  for (j in 1 : iter){
    mcSum <- largestSbmxShabalin(poissonMatrix(M, N, 0, 0, 0), m, n)
    pvalue <- pvalue + (mcSum >= originSum) / (iter + 1)
  }
  mcTest = (pvalue <= alpha) + 0
  return(list('pvalue' = pvalue, 'test' = mcTest))
  
}
