\name{lacv}
\alias{lacv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Computes localized (wavelet) autocovariance function}
\description{Compute the LACV function for a locally
	stationary wavelet process. 
}
\usage{
lacv(x, filter.number = 1, family = "DaubExPhase", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The time series you want to compute the LACV for}
  \item{filter.number}{The wavelet that you wish to compute the LACV
	with respect to}
  \item{family}{The wavelet family}
  \item{\dots}{ Additional arguments to the spectrum computation
	contained within}
}
\details{A locally stationary wavelet process is a particular kind
	of non-stationary time series constructed out of wavelet
	atoms, with a time-varying spectrum (slowly varying).
	This kind of model is useful for time series whose spectral
	properties change over time. 

	The time-varying spectrum can be computed from within the
	WaveThresh library by the \code{ewspec} function. However,
	just as in the classical stationary case, where the
	spectrum and autocovariance are a Fourier transform pair, the
	paper Nason, von Sachs, Kroisandt (2000) [NvSK2000] shows that the
	evolutionary wavelet spectrum is paired to a localized
	autocovariance function using a wavelet-like transform.
	This is expressed in formula (14) of the NvSK2000 paper.

	This function computes the localized autocovariance by
	first computing the estimate of the evolutionary spectrum,
	and then directly transforming it using formula (14) via
	the autocorrelation wavelet transform.
}
\value{A matrix that contains the localized autocovariance.
	If the original time series was of length T, then the number
	of rows of the returned matrix is also T, one row for each time
	point.
	The columns of the array correspond to the lag. The number of
	columns, 2K+1,  depends both on the length of the time series and
	also the order of the wavelet (smoother wavelets return
	lacv matrices with larger number of lags). Lag 0 is always
	the centre column, with negative lags from -K to -1 are
	the leftmost columns, lags from 1 to K are the rightmost columns.
}
\references{`Costationary and stationarity tests for stock index returns' by Car
dinali and Nason}
\author{Guy Nason}
\seealso{\code{ewspec}}
\examples{
#
# Generate an AR(1) time series
#
vv <- arima.sim(model=list(ar=0.8), n=1024)
#
# Compute the ACF of this stationary series
#
vv.acf <- acf(vv, plot=FALSE)
#
# Compute the localized autocovariance. We'll use
# a reasonably smooth wavelet.
#
\dontrun{vv.lacv <- lacv(vv, filter.number=4)}
#
# Various plots of the lacv
#
# The whole lacv at time t=100. First work out sizes:
#
\dontrun{dim(vv.lacv)}
# [1] 1024 14323
#
(14323-1)/2
# [1] 7161
#
# So, rows of vv.lacv index time ranging from 1 to 1024
# Cols index lag ranging from -7161 through to 7161.
#
# Now the plot
#
\dontrun{plot( -7161:7161, vv.lacv[100,], type="l", xlab="lag", ylab="Autocovariance")}
#
# Most of the action, for this time series especially, is near to the zero
# lag. So, let's zoom in on the action. And let's plot autocorrelation,
# rather than autocovariance. We'll use the estimate of the autocovariance
# at lag zero at this time for the denominator.
#
# The following commands plots the lacv for lags ranging from -10 to 10 at
# time t=100. Change the 100 to something else for different times
#
\dontrun{plot(-10:10,vv.lacv[100,(7162-10):(7162+10)]/vv.lacv[100,7162], type="l", xlab="lag")}
#
# Let us get an indication of how accurate the lacv is, we shall superimpose
# some of the regular acf values on this plot. Note that vv.acf[1] contains
# the acf at lag zero, which is 1.
# 
\dontrun{points(1, vv.acf[2])}
\dontrun{points(2, vv.acf[3])}
\dontrun{points(3, vv.acf[4])}
\dontrun{points(4, vv.acf[5])}
#
# You should find that the lacv is pretty accurate, which is good
# seeing as it is a local quantity.
#
} 
\keyword{ts}
