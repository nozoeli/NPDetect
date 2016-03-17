normalMatrix = function(M, N, m, n, mu){
	# M, N : dimensions of the matrix
	# m, n : dimensions of the submatrix
	# mu : positive mean
	# Creates a random matrix where the entries are independent normal with unit variance and mean 0, except for the top-left submatrix (of given size) where the mean is mu
	A = matrix(rnorm(M*N), M, N)
	A[1:m, 1:n] = A[1:m, 1:n] + mu
	return(A)
	}


poissonMatrix <- function(M, N, m, n, mu){
  # Generalize centered poisson distribution according to the standard exponential model
  # M, N : dimension of the matrix
  # m, n : dimension of the submatrix
  # mu stands for the mean of the elevated matrix. mu >= 1 required.
  # The base distribution is Poi(1) - 1
  mx1 <- c(rep(sqrt(mu - 1), m), rep(0, M - m)) %*% t(c(rep(sqrt(mu - 1), n), rep(0, N - n)))
  mx <- matrix(rpois(M * N, as.vector(mx1 + 1)), M, N) - 1
  return(mx)
}