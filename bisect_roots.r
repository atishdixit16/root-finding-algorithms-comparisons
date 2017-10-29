bisect_roots <- function(point1, point2) {
	epsilon <- 1e-4
	if ( (udf(point1) * udf(point2) ) > 0  ) {
		cat('The functions of initial points should have diffrent signs ...\n')
		return(FALSE)
	}
	center <- ( point1 + point2 ) / 2
	cat(center,' ')
	if (abs(point1 - point2) < epsilon ) {
		if (udf(center) < epsilon) {
			cat(center,'\n')
			return(center)
		}
		else {
			cat('Too complex function ...\n')
			return(FALSE)
		}
	}
	if (udf(point1)*udf(center) < 0 )
		bisect_roots(point1,center)
	if (udf(point2)*udf(center) < 0 )
		bisect_roots(center,point2)
}

udf <- function(x) {
	ans <- exp(-x) - x
	return(ans)
}

