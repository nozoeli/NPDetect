source('testFunctions.R')

#------------------Model Initialization---------
M <- 100
N <- 100
m <- 20
n <- 30  # Matrix and elevated submatrix sizes
mu <- 0.8 
mupoi <- exp(mu)  # Elevated parameter in the anomaly

#-----------------Data Generation----------------
dataNormal <- normalMatrix(M, N, m, n, mu) 
# Genenrate M*N matrix with one m*n submatrix. Entries in the submatrix is N(0,mu) and the rest is N(0,1)
dataPoi <- poissonMatrix(M, N, m, n, mupoi)
# Genenrate M*N matrix with one m*n submatrix. Entries in the submatrix is Poi(exp(mu)) - 1 and the rest is Poi(1) - 1

#-----------------Tests--------------------------
testMonteCarlo(dataNormal, m, n)  # Monte Carlo test for matrix with Normal entries
testMonteCarloPoi(dataPoi, m, n)  # Monte Carlo test for matrix with Poisson entries

biPermuTest(dataNormal, m, n)  # Bidimensional permutation test
uniPermuTest(dataPoi, m, n)  # Unidimensional (by row) permutation test