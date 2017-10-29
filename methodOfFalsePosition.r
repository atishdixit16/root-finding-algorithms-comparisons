methodOfFalsePosition <- function(point1, point2) {
	epsilon <- 1e-4
	if ( (udf(point1) * udf(point2) ) > 0  ) {
		cat('The functions of initial points should have diffrent signs ...\n')
		return(NULL)
	}
	center <- ( point1*udf(point2) - point2*udf(point1) ) / (udf(point2) - udf(point1))
	cat (center,' ' )
	if (abs(point1 - point2) < epsilon ) {
		if (abs(udf(center)) < epsilon) {
			return(center)
		}
		else {
			cat('Too complex function ...\n')
			return(NULL)
		}
	}
	if (udf(point1)*udf(center) < 0 )
		methodOfFalsePosition(point1,center)
	else 
		methodOfFalsePosition(center,point2)
}

udf <- function(x) {
	ans <- exp(-x) - x
	return(ans)
}

