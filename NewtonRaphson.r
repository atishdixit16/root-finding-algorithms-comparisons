NewtonRaphson <- function(guess) {
	epsilon <- 1e-4
	cat(guess, ' ' )
	nextGuess <- guess - ( udf(guess) / diffudf(guess) )
	if (abs(guess - nextGuess) < epsilon  )
		return(nextGuess)
	NewtonRaphson(nextGuess)
}

udf <- function(x) {
        return(exp(-x) -x)
}

diffudf <- function(x) {
        return(-exp(-x)-1)
}

