`myTOS` <-
function(x, Bsims=100, WPsmooth=TRUE, verbose=TRUE, plot.avspec=FALSE,
	plot.avsim=FALSE, theTS = TOSts){


#
# Compute EWS estimate of x
#
xews <- ewspec(x, WPsmooth=WPsmooth, smooth.dev=var)$S
#
# Compute Test Statistic for the data, x
#
TS <- theTS(xews)
#
# Now compute mean spectrum (or the null spectrum)
#
J <- nlevels(xews)
n <- length(x)
m <- matrix(xews$D, nrow=J, ncol=n, byrow=TRUE) # Turn spec into matrix
m <- apply(m, 1, mean) # Vector of means one for each scale
m[m < 0] <- 0
m <- matrix(m, nrow=J, ncol=n)
xavspec <- xews
xavspec$D <- as.vector(t(m))

if (plot.avspec==TRUE)
	plot(xavspec)
#
# Now do bootstrap simulations. Generate time series with spectrum xavspec,
# compute test statistic and store.
#

for(b in 2:Bsims)	{
	xbs <- LSWsim(xavspec)
	if (plot.avsim==TRUE)
		ts.plot(xbs)
	xbs.ews <- ewspec(xbs, WPsmooth=WPsmooth, smooth.dev=var)$S
	TS <- c(TS, theTS(xbs.ews))
	if (verbose==TRUE)
		cat(".")
	}

if (verbose==TRUE)
	cat("\n")

TS




}

