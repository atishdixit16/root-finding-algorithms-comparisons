fixedPoint <- function(guess) {
	if (diffudf(guess) >= 1) {
		cat('differentiation of fixed point function must be less than 1..\n')
		return(NULL)
	}
	epsilon <- 1e-4
	cat(guess,' ')
	nextGuess <- udf(guess)
	if (abs(guess - nextGuess) < epsilon  )
		return(nextGuess)
	fixedPoint(nextGuess)
}

udf <- function(x) {
	return(exp(-x) )
}

diffudf <- function(x) {
	return(-exp(-x) )
}
