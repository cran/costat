\name{EWSsmoothRM}
\alias{EWSsmoothRM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perform running mean smoothing of an EWS object
}
\description{Performs running mean smoothing of bandwidth s of
	an EWS, such as that returned by the \code{ewspec} function
	of \code{wavethresh}. 
}
\usage{
EWSsmoothRM(S, s)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{The spectrum to smooth
}
  \item{s}{The bandwidth (or number of ordinates to include in the
	running mean) }
}
\details{Each level of the EWS is subject to a running mean smooth.
	After smoothing a level the resultant smooth is shorter than
	the original level (due to the mean not being able to
	overlap the boundaries). This deficit is made up by augmenting
	the start of the smooth with a right number of smoothed values
	taken from the first smoothed value.
}
\value{A EWS object contained in a \code{wd} object of type "station"
which contains the smoothed spectrum.
}
\references{Cardinali, A. and Nason, Guy P. (2013) Costationarity of
	Locally Stationary Time Series Using costat.
	\emph{Journal of Statistical Software}, \bold{55}, Issue 1.

Cardinali, A. and Nason, G.P. (2010) Costationarity of locally stationary
time series. \emph{J. Time Series Econometrics}, \bold{2}, Issue 2, Article 1.
}
\author{
G.P. Nason
}

\seealso{\code{\link{lacv}}
}
\examples{
#
# Make dummy time series
#
x <- rnorm(128)
#
# Compute spectrum, but don't do smoothing
#
xews <- ewspec(x, WPsmooth=FALSE)$S
#
# Now smooth the spectrum using running mean smoothing with bandwidth of 5
#
ans <- EWSsmoothRM(xews, s=5)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{smooth} 
\keyword{ts}
