`LCTSres` <-
function(res, tsx, tsy, inc=0, solno=1:nrow(res$endpar), filter.number=1, family="DaubExPhase", plot.it=FALSE,
	spec.filter.number=1, spec.family="DaubExPhase",plotcoef=FALSE,
	sameplot=TRUE, norm=FALSE, plotstystat=FALSE, plotsolinfo=TRUE, onlyacfs=FALSE, acfdatatrans=I, xlab="Time", ...){

nopt <- nrow(res$endpar)

lcfs <- ncol(res$endpar)
lts <- length(tsx)

xx <- inc+1:lts


cat("solno is ", solno, "\n")

for(i in 1:nopt)	{


	if (!is.na(match(i, solno)))	{
		cat(i, "\n")

	if (res$convergence[i]==0)	{
		cfs <- res$endpar[i,]
		
		alpha <- cfs[1:(lcfs/2)]
		beta <- cfs[(lcfs/2+1):lcfs]

		v <- coeftofn(alpha=alpha, beta=beta, n=lts,
			filter.number=filter.number, family=family)

		lcts <- v$alpha*tsx + v$beta*tsy


		lctsspec <- ewspec(lcts, filter.number=spec.filter.number,
			family=spec.family)$S


		if (plotcoef==TRUE)	{
			if (sameplot==TRUE)	{
				if (norm==TRUE)	
					v$alpha <- v$alpha/sqrt(sum(v$alpha^2))
				if (i==1)	{
					plot(xx, v$alpha, main="alpha", type="l", xlab=xlab, ylab="alpha_t")
					}
				else	{
					lines(xx, v$alpha)
					}

				}
			else	{
				plot(xx, v$alpha, main="alpha", type="l",
					xlab=xlab)
				#ts.plot(v$beta, main="beta")
				}
			}

		else	{
			if (plotsolinfo==TRUE)	{
			plot(xx, v$alpha, main="alpha", type="l", xlab=xlab, ylab="alpha_t")
			plot(xx, v$beta, main="beta", type="l", xlab=xlab, ylab="beta_t")
		plot(xx, lcts, xlab=xlab, type="l", main=paste("Combined. Minvar: ", signif(res$minvar[i],3)), ylab="Z_t")
		plot(lctsspec, main=paste("p-val is", signif(res$pvals[i],3)),
			xlab=xlab, sub="", ylab="Scale j")
			}
		scan()
		if (plotstystat==TRUE)	{
			if (!onlyacfs)
				ts.plot(lcts, main="Time series")
			acf(acfdatatrans(lcts), ...)
			acf(acfdatatrans(lcts), type="partial", ...)
			if (!onlyacfs)
				spectrum(lcts, span=c(5,7))
			scan()
			}
		}
		
			}
		}
	}
lcts
}

