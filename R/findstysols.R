`findstysols` <-
function(Nsims=100, Ncoefs=3, tsx, tsy, sf=100, plot.it=FALSE, print.it=FALSE, verbose=FALSE,
	lctsfn=LCTS, prodcomb.fn=prodcomb, filter.number=1, family="DaubExPhase", my.maxit=500){


endpar <- startpar <- matrix(nrow=Nsims, ncol=2*Ncoefs)
convergence <- minvar <- pvals <- rep(0, Nsims)

for(i in 1:Nsims)	{
	cat("Optimization ", i, "\n")
	ipar <- rnorm(2*Ncoefs)
	startpar[i,] <- ipar
	tmpo <- optim(par=ipar,fn=lctsfn, tsx=sf*tsx, tsy=sf*tsy, plot.it=plot.it, filter.number=filter.number, family=family, control=list(maxit=my.maxit, reltol=1e-6))
	endpar[i,] <- tmpo$par
	convergence[i] <- tmpo$convergence
	minvar[i] <- tmpo$value
	tmpo2 <- prodcomb.fn(tmpo$par,tsx=tsx,tsy=tsy, filter.number=filter.number, family=family)
	tmpo3 <- myTOS(tmpo2)
	pvals[i] <- plotBS(tmpo3, plot=plot.it, verbose=verbose)
	}
l <- list(startpar=startpar, endpar=endpar, convergence=convergence,
	minvar=minvar, pvals=pvals)
l
}

