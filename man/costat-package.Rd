\name{costat-package}
\alias{costat-package}
\alias{costat}
\docType{package}
\title{Computes localized autocovariance and searches for costationary
	solutions to bivariate time series.
}
\description{Computes a time-varying autocovariance and associated
	plots for plotting this. Also can search for costationary
	solutions between two time series.
}
\details{
\tabular{ll}{
Package: \tab costat\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2012-10-24\cr
License: \tab What license is it under?\cr
}
}
\author{Guy Nason, <g.p.nason@bristol.ac.uk>
}
\references{
Cardinali, A. and Nason, Guy P. (2013) Costationarity of
	Locally Stationary Time Series Using costat.
	\emph{Journal of Statistical Software}, \bold{55}, Issue 1.

Cardinali, A. and Nason, G.P. (2010) Costationarity of locally stationary
time series. \emph{J. Time Series Econometrics}, \bold{2}, Issue 2, Article 1.
}
\keyword{ts}
\seealso{\code{\link{findstysols}}, \code{\link{lacv}}
}
\examples{
#
# Compute localized acv
#
x <- c(rnorm(128, sd=1), rnorm(128, sd=3))
xlacv <- lacv(x, lag.max=30)
#
# Plot the time-varying autocovariance at time t=100
#
\dontrun{plot(xlacv, type="acf", the.time=100, plotcor=FALSE)}
#
# Plot the time-varying autocovariance at time t=400
#
\dontrun{plot(xlacv, type="acf", the.time=400, plotcor=FALSE)}
#
# See examples for findstysols for other examples
#
}
