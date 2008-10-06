\name{tstosscan}
\alias{tstosscan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Perform a series of stationarity tests on a time series
	at different spans and locations.}
\description{A global stationarity test is very useful to examining
	a whole series to see whether it there is evidence for
	stationarity. However, sometimes it can be useful to
	apply the test to portions of the time series. This
	function does this in an organized way by applying the test
	to successive, overlapping, portions of a time series,
	and doing this for different widths of portions (spans).
}
\usage{
tstosscan(ts, spans, Bsims = 100, skipby = 1, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ts}{The time series to analyze}
  \item{spans}{A vector containing the spans to analyze}
  \item{Bsims}{The number of bootstrap simulations for each stationarity
		test.}
  \item{skipby}{How far to move the portion for each significance test.}
  \item{verbose}{If TRUE then print out informative messages as the
	function proceeds.}
}
\details{A span or portion of the time series is given by
	\deqn{X_t} to \deqn{X_{t+span}}. The tests of stationarity are applied
	to portions such as these. Each test is applied to a different
	portion ranging from t=1, in steps of \code{skipby}, 
	to the highest possible value t so that t+span does not overlap
	the end of the series.

	The function can accept a vector of spans, and the function
	runs processing one span at a time.

	Note that this procedure is an example of a multiple hypothesis
	test. The tests are all performed individually with the same
	p-value and there is no correction here for multiple testing.
}
\value{A list containing the following components:
  \item{spans}{A copy of the input spans vector which contains the list
		of portion widths}
  \item{pvallist}{A list of length \code{length{spans}} (ie there is
	a list component for each span). For each span there is a
	vector containing the p-values for each test, for each step}
  \item{starlist}{A list of length \code{length{spans}}, the same
	format as \code{pvallist} except that each entry contains the
	starting index of the portion. So from the starting index,
	and the span, the coordinates of the whole time series portion
	can be obtained.}
}
\references{`Costationary and stationarity tests for stock index returns' by Car
dinali and Nason, 2008, University of Bristol Technical Report 08:08.}
\author{Guy Nason}
\seealso{\code{\link{plottstosscan}}, \code{\link{myTOS}}}
\examples{
#
# Compute tests of stationarity on white noise Gaussian
# (not run on installation because its compute intensive)
#
v <- rnorm(128)
\dontrun{v.tstos <- tstosscan(v, spans=c(32,64))}

\dontrun{plottstosscan(v, v.rstos, inc=0)}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts} 
