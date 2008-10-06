`prodcomb` <-
function(cfs, tsx, tsy, filter.number=1, family="DaubExPhase", all=FALSE){

lcfs <- length(cfs)
lts <- length(tsx)


alpha <- cfs[1:(lcfs/2)]
beta <- cfs[(lcfs/2+1):lcfs]

v <- coeftofn(alpha=alpha, beta=beta, n=lts, filter.number=filter.number,
	family=family)

lcts <- v$alpha*tsx + v$beta*tsy

if (all==FALSE)
	return(lcts)
else	{
	l <- list(lcts=lcts, alpha=v$alpha, beta=v$beta)
	return(l)
	}
}

