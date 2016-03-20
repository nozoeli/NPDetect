# NPDetect
R codes for the paper "Distribution-free Detection of a Submatrix“ by Ery Arias-Castro and Yuchao Liu

File Description
- example.R : Example R code for illustrating the usage of functions used here.
- shabalin.R : Provides functions associated with the local hill-climbing algorithm (Shabalin et al, (2009)) for finding the certain sized submatrix with the largest entry sum. 
- randomMatrx.R : Functions generating random matrix with one submatrix with elevated parameter. Functions generating Normal and Poisson entries are provided.
- testFunctions.R : Provides functions for testing purposes. Unidimensional / Bidimensional scan test function and Monte carlo test for Normal and Poisson families are provided.
- simulationNormal10/20.R : Generates random matrix with normal entries and elevated anomaly, and compares the testing result between unidimensional / bidimensional scan test and Monte Carlo.
- simulationPoisson10/20.R : Generates random matrix with Poisson entries and elevated anomaly, and compares the testing result between bidimensional scan test and Monte Carlo.
- simulationRankUni/Bi.R : Generates random matrix with normal entries and elevated anomaly, and compares the testing result between unidimensional / bidimensional rank scan test and Monte Carlo.
- evaluation.R : Generates graphs illustrating the simulation results. R package 'ggplot2' is needed.

Reference:

Shabalin, A. A., V. J. Weigman, C. M. Perou, and A. B. Nobel (2009). Finding large average submatrices in high dimensional data. The Annals of Applied Statistics 3 (3), 985-1012. (http://arxiv.org/abs/0905.1682)
