lagrange <- function(x,xdata,ydata) {
	sum <- 0
	for (i in 1:length(xdata)) {
		prod = 1;
		for (j in 1:length(xdata)) {
			if (i!=j) {
				prod <- prod * ( (x-xdata[j]) / (xdata[i]-xdata[j]) )
			}
		}
		sum <- sum + ydata[i]*prod
	}
	return(sum)
}
