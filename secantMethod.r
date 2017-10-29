secantMethod <- function(point1, point2) {
	epsilon <- 1e-4
	pointNew <- point1 - udf(point1)*( (point2-point1)/(udf(point2) - udf(point1)) )
	cat(pointNew,' ')
	if (abs(udf(pointNew)) < epsilon )
		return(pointNew)
	else
		secantMethod(point2,pointNew)
}

udf <- function(x) {
	ans <- exp(-x) - x
	return(ans)
}

