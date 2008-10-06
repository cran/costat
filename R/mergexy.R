`mergexy` <-
function(l){


nl <- length(l)

startpar <- endpar <- convergence <- minvar <- pvals <- NULL

for(i in 1:nl)	{

	xy <- get(l[i])
	startpar <- rbind(startpar, xy$startpar)
	endpar <- rbind(endpar, xy$endpar)
	convergence <- c(convergence, xy$convergence)
	minvar <- c(minvar, xy$minvar)
	pvals <- c(pvals, xy$pvals)
	}

l <- list(startpar=startpar, endpar=endpar, convergence=convergence,
	minvar=minvar, pvals=pvals)
l


}

