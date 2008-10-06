`tstosscan` <-
function(ts, spans, Bsims=100, skipby=1, verbose=FALSE){

nspans <- length(spans)
lts <- length(ts)

starlist <-pvallist <- vector("list", nspans)


for(i in 1:nspans)	{

	if (verbose==TRUE)
		cat("Span ", i, spans[i], "\n")

	thespan <- spans[i]
	startvec <- seq(from=1, to=(lts-thespan+1), by=skipby)
	pvalvec <- NULL

	for(j in startvec)	{

		if (verbose==TRUE)	
			cat("Do test piece  [", j, ",", j+thespan-1, "]\n")
		piece <- ts[j:(j+thespan-1)]
		pieceTOS <- myTOS(piece, Bsims=Bsims)
		pvalvec <- c(pvalvec, plotBS(pieceTOS, plot=FALSE, verbose=FALSE))
		}
	pvallist[[i]] <- pvalvec
        starlist[[i]] <- startvec 
	}
	
l <- list(spans=spans, pvallist=pvallist, starlist=starlist)
l

}

