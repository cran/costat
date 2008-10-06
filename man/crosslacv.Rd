\name{crosslacv}
\alias{crosslacv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute the time-localized cross-autocovariance of two time series}
\description{Compute the (wavelet based) time-localized cross-autocovariance
	of two time series. This is the quantity
	\deqn{\hat{c}^{XY}} described at the end of Section 2 of
	Cardinali and Nason, 2008.
}
\usage{
crosslacv(x, y, filter.number = 1, family = "DaubExPhase", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{One of the time series (dyadic length)}
  \item{y}{The other time series (dyadic length)}
  \item{filter.number}{The wavelet filter number for the spectral analysis}
  \item{family}{The wavelet family for the spectral analysis.}
  \item{\dots}{Other arguments for the call to \code{\link{ewcrossspec}}}
}
\details{This function works in almost exactly the same way as
	\code{\link{lacv}} except it computes a cross localized autocovariance
	for two time series rather than a localized autocovariance for
	a single time series. See the help page for \code{\link{lacv}}.
}
\value{A matrix containing the localized cross autocovariance. 
        If the original time series was of length T, then the number
        of rows of the returned matrix is also T, one row for each time
        point.
        The columns of the array correspond to the lag. The number of
        columns, 2K+1,  depends both on the length of the time series and
        also the order of the wavelet (smoother wavelets return
        crosslacv matrices with larger number of lags). Lag 0 is always
        the centre column, with negative lags from -K to -1 are
        the leftmost columns, lags from 1 to K are the rightmost columns.
}
\references{`Costationary and stationarity tests for stock index returns' by Car
dinali and Nason, 2008, University of Bristol Technical Report 08:08.}
\author{Guy Nason}
\seealso{\code{\link{ewcrossspec}}, \code{\link{lacv}}}
\examples{
#
# Example of use
#
\dontrun{x2y2.crosslacv <- crosslacv(x2, y2)}
#
# Can plot x2y2.crosslacv in exactly the same way as for the return values
# of lacv.
#
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
