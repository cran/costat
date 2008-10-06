\name{CrossWP}
\alias{CrossWP}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute raw cross wavelet periodogram (not intended for casual use)}
\description{Computes the raw cross wavelet periodogram for two time series.
}
\usage{
CrossWP(xwdS, ywdS)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xwdS}{The time-ordered nondecimated wavelet transform of one time series, x}
  \item{ywdS}{The time-ordered nondecimated wavelet transform of another time series, y}
}
\details{Just the component-wise product of the two nondecimated transforms
	of two time series. Formula is given by definition 3 in
	Cardinali and Nason (2008).
}
\value{A time-ordered nondecimated wavelet transform object containing
	the raw cross wavelet periodogram of two time series.
}
\references{`Costationary and stationarity tests for stock index returns' by Car
dinali and Nason}
\author{Guy Nason}
\seealso{\code{\link{ewcrossspec}}}
\examples{
#
# See code of ewcrossspec to see how this function is called.
#
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
