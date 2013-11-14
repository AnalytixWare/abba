permute = function(n) {
	if (length(n) > 1 ) {
		n = max(n)
	}
	return(order(runif(n)))
}