\name{TOSts}
\alias{TOSts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{A test statistic for stationarity}
\description{The \eqn{T_{\mbox{vS}}}{T_{vS}} test statistic from
	the Cardinali and Nason article. Measures the degree of
	non-stationarity using the estimated evolutionary wavelet
	spectrum (EWS)
}
\usage{
TOSts(spec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{spec}{An EWS estimate, e.g. from the \code{$S} component from
	\code{ewspec}}
}
\details{Given an EWS estimate. This computes the sample variance of
	the estimate for each scale level and then returns the sum
	of these variances.
}
\value{A single number which is the sum of the sample variances of
	each scale level from an EWS estimate. If the EWS estimate is
	constant for each scale then the return value is zero. 
}
\references{Cardinali, A. and Nason, Guy P. (2013) Costationarity of
	Locally Stationary Time Series Using costat.
	\emph{Journal of Statistical Software}, \bold{55}, Issue 1.

Cardinali, A. and Nason, G.P. (2010) Costationarity of locally stationary
time series. \emph{J. Time Series Econometrics}, \bold{2}, Issue 2, Article 1.
}
\author{Guy Nason}
\seealso{\code{\link{BootTOS}}}
\examples{
#
# Compute a spectral estimate on an sample time series (just use iid data)
#
xsim <- rnorm(128)
xews <- ewspec(xsim, smooth.dev=var)$S
#
# You could plot this spectral estimate if you liked
#
\dontrun{plot(xews)}
#
# Compute test statistic
#
TOSts(xews)
#[1] 0.1199351
#
# Although the time series x here is a realization from a stationary process
# the test statistic is not zero (this is because of the estimation error
# inherent in this small sample). 
#
# This is why the bootstrap test, \code{\link{BootTOS}} is required to
# assess the significance of the test statistic value.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
