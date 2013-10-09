\name{lacv}
\alias{lacv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Computes localized (wavelet) autocovariance function}
\description{Compute the LACV function for a locally
	stationary wavelet process. 
}
\usage{
lacv(x, filter.number = 10,
	family = c("DaubExPhase", "DaubLeAsymm"), smooth.dev=var,
	AutoReflect=TRUE, lag.max=NULL, smooth.RM=0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The time series you want to compute the LACV for}
  \item{filter.number}{The wavelet that you wish to compute the LACV
	with respect to}
  \item{family}{The wavelet family}
  \item{smooth.dev}{The deviance used in smoothing if running mean smoothing
	is not used, ie in the call to \code{ewspec}.}
  \item{AutoReflect}{If TRUE then the spectrum is computed on a boundary-corrected series, overcoming the lack of periodicity in the time series.}
  \item{lag.max}{The maximum lag that the function computes. If this option
	is \code{NULL} then the largest possible will be computed and used}
  \item{smooth.RM}{If this is zero then regular wavelet smoothing of the
	periodogram will be used. If not zero then running mean smoothing
	of the periodogram will be used with a bandwidth given by
	this argument.}
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
\value{An object of class \code{lacv}. This is a list with the following
	components: \code{lacv} which is a matrix that contains the localized
	autocovariance.
	If the original time series was of length T, then the number
	of rows of the returned matrix is also T, one row for each time
	point.
	The columns of the array correspond to the lag. The number of
	columns, 2K+1,  depends both on the length of the time series and
	also the order of the wavelet (smoother wavelets return
	lacv matrices with larger number of lags). Lag 0 is always
	the centre column, with negative lags from -K to -1 are
	the leftmost columns, lags from 1 to K are the rightmost columns;
	\code{lacr}: a matrix, with the same dimensions as \code{lacv}
	but containing the local autocorrelations; \code{date}: the date
	this function was executed.
}
\references{Cardinali, A. and Nason, Guy P. (2013) Costationarity of
	Locally Stationary Time Series Using costat.
	\emph{Journal of Statistical Software}, \bold{55}, Issue 1.

Cardinali, A. and Nason, G.P. (2010) Costationarity of locally stationary
time series. \emph{J. Time Series Econometrics}, \bold{2}, Issue 2, Article 1.

Nason, G.P., von Sachs, R. and Kroisandt, G. (2000) Wavelet
	processes and adaptive estimation of the evolutionary wavelet
	spectrum. \emph{J. R. Statist. Soc. B}, \bold{62}, 271-292.
}
\author{Guy Nason}
\seealso{\code{ewspec}, \code{\link{print.lacv}}, \code{\link{plot.lacv}},
	\code{\link{summary.lacv}}}
\examples{
#
# Generate an AR(1) time series
#
vsim <- arima.sim(model=list(ar=0.8), n=1024)
#
# Compute the ACF of this stationary series
#
vsim.acf <- acf(vsim, plot=FALSE)
#
# Compute the localized autocovariance. We'll use
# a reasonably smooth wavelet.
#
vsim.lacv <- lacv(vsim, filter.number=4, lag.max=30)
#
# Now plot the time-varying autocorrelations, only the first 5 lags
#
\dontrun{plot(vsim.lacv, lags=0:5)}
#
# Now plot the localized autocorrelation at time t=100, a plot similar
# to the usual R acf plot.
#
\dontrun{plot(vsim.lacv, type="acf", the.time=100)}
} 
\keyword{ts}
