\name{LCTS}
\alias{LCTS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Computes a Linear Combination Test Statistics}
\description{Given a particular linear combination, specified in terms of
	coefficients, cfs, this functions forms the linear combination of
	two time series, tsx, tsy and returns the result of a stationarity
	test statistic on the combination.
}
\usage{
LCTS(cfs, tsx, tsy, filter.number = 1,
	family = c("DaubExPhase", "DaubLeAsymm"), plot.it = FALSE,
        spec.filter.number = 1,
	spec.family = c("DaubExPhase", "DaubLeAsymm"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cfs}{Coefficients describing the linear combination vectors.
	The first half correspond to the first vector (alpha) the second
	half to the beta vector. Hence this vector must have an even length,
	and each half has a length a power of two minus one.}
  \item{tsx}{The x time series}
  \item{tsy}{The y time series}
  \item{filter.number}{This function turns the coefficients into a linear
	combination function (e.g. alpha). This argument specifies the
	filter.number of the inverse wavelet transform that turns coefficients	
	into a lc function.}
  \item{family}{Same as filter.number but for the wavelet family}
  \item{plot.it}{If TRUE then various things are plotted: both of the linear
	combination vectors/time series, the combined time series and
	its EWS estimate}
  \item{spec.filter.number}{The wavelet filter used to compute the EWS
	estimate}
  \item{spec.family}{The wavelet family used to compute the EWS estimate}
}
\details{This function forms a time-varying linear combination of two
	times series to form a third time series. Then a `stationarity
	test' test statistic is applied to the third time series to
	compute how stationary (or non-stationary it is). This function
	is called by \code{\link{findstysols}} and actually does the work
	of forming the lc of two time series and gauging the stationarity}
\value{A single number which is the value of the test of stationarity
	for the combined time series. This is the result of \code{\link{TOSts}}
	but normalized for the squared coefficient norm}

\references{Cardinali, A. and Nason, Guy P. (2013) Costationarity of
	Locally Stationary Time Series Using costat.
	\emph{Journal of Statistical Software}, \bold{55}, Issue 1.

Cardinali, A. and Nason, G.P. (2010) Costationarity of locally stationary
time series. \emph{J. Time Series Econometrics}, \bold{2}, Issue 2, Article 1.
}
\author{Guy Nason}
\seealso{\code{\link{findstysols}}, \code{\link{TOSts}},
	\code{\link{coeftofn}}}
\examples{
#
# Apply this function to random combination coefficients.
#
# The combination coefficients: comprised of two vectors each of length 3
# Note that 3 = 2^2 - 1, vectors need to be of length a power two minus 1 
#
#	sret, fret are two time series in the package
#
data(sret)
data(fret)
LCTS( c(rnorm(3), rnorm(3)), sret, fret)
#[1] 1.571728e-13
#
# The value of the test statistic is 1.57e-13
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
