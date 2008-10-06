`LCTS` <-
function(cfs, tsx, tsy, filter.number=1, family="DaubExPhase", plot.it=FALSE,
	spec.filter.number=1, spec.family="DaubExPhase"){

lcfs <- length(cfs)
lts <- length(tsx)

cfsnorm <- sum((cfs)^2)

alpha <- cfs[1:(lcfs/2)]
beta <- cfs[(lcfs/2+1):lcfs]

v <- coeftofn(alpha=alpha, beta=beta, n=lts, filter.number=filter.number,
	family=family)

lcts <- v$alpha*tsx + v$beta*tsy


lctsspec <- ewspec(lcts, filter.number=spec.filter.number,
		family=spec.family)$S


ans <- TOSts(lctsspec)/(cfsnorm^2)

if (plot.it==TRUE)	{
	ts.plot(v$alpha, main="alpha")
	ts.plot(v$beta, main="beta")
	ts.plot(lcts, main="Combined")
	plot(lctsspec, main=paste("Var is", signif(ans,3)))
	}
ans
}

