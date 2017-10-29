center <- NULL
bisect_roots <- function(point1, point2) {
        epsilon <- 1e-4
        if ( (udf(point1) * udf(point2) ) > 0  ) {
                cat('The functions of initial points should have diffrent signs ...\n')
                return(FALSE)
        }
        center <- c(center, ( point1 + point2 ) / 2)
        if (abs(point1 - point2) < epsilon ) {
                if (udf(center[length(center)]) < epsilon) {
                        return(center)
                }
                else {
                        cat('Too complex function ...\n')
                        return(FALSE)
                }
        }
        if (udf(point1)*udf(center[length(center)]) < 0 )
                bisect_roots(point1,center[length(center)])
        if (udf(point2)*udf(center[length(center)]) < 0 )
                bisect_roots(center[length(center)],point2)
}

nextGuess <- NULL
fixedPoint <- function(guess) {
	if (length(nextGuess == 0)) {
        	if (fpdiffudf(guess) >= 1) {
                	cat('differentiation of fixed point function must be less than 1..\n')
	                return(FALSE)
        	}
	}
        epsilon <- 1e-4
        nextGuess <- c(nextGuess ,fpudf(guess))
        if (abs(guess - nextGuess[length(nextGuess)]) < epsilon  )
                return(nextGuess)
        fixedPoint(nextGuess[length(nextGuess)])
}

center <- NULL
methodOfFalsePosition <- function(point1, point2) {
        epsilon <- 1e-4
        if ( (udf(point1) * udf(point2) ) > 0  ) {
                cat('The functions of initial points should have diffrent signs ...\n')
                return(NULL)
        }
        center <-c(center, ( point1*udf(point2) - point2*udf(point1) ) / (udf(point2) - udf(point1)))
        if (abs(point1 - point2) < epsilon ) {
                if (abs(udf(center[length(center)])) < epsilon) {
                        return(center)
                }
                else {
                        cat('Too complex function ...\n')
                        return(NULL)
                }
        }
        if (udf(point1)*udf(center[length(center)]) < 0 )
                methodOfFalsePosition(point1,center[length(center)])
        else
                methodOfFalsePosition(center[length(center)],point2)
}

nextGuess <- NULL
NewtonRaphson <- function(guess) {
        epsilon <- 1e-4
        nextGuess <- c( nextGuess  , guess - ( udf(guess) / diffudf(guess) ) )
        if (abs(guess - nextGuess[length(nextGuess)]) < epsilon  )
                return(nextGuess)
        NewtonRaphson(nextGuess[length(nextGuess)])
}

pointNew <- NULL
secantMethod <- function(point1, point2) {
        epsilon <- 1e-4
        pointNew <-  c( pointNew , (  point1 - udf(point1)*( (point2-point1)/(udf(point2) - udf(point1)) ) ) )
        if (abs(udf(pointNew[length(pointNew)])) < epsilon )
                return(pointNew)
        else
                secantMethod(point2,pointNew[length(pointNew)])
}

udf <- function(x) {
        return(exp(-x) - x)
}

diffudf <- function(x) {
        return(-exp(-x) - 1)
}


fpudf <- function(x) {
        return(exp(-x))
}

fpdiffudf <- function(x) {
        return(-exp(-x))
}


br <- bisect_roots (0, 5)
fp <- fixedPoint (5)
mfp <- methodOfFalsePosition (0, 5)
nr <- NewtonRaphson (5)
sm <- secantMethod (0, 5) 
