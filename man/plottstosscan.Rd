\name{plottstosscan}
\alias{plottstosscan}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot the results of a multiple stationarity test on a time series}
\description{The \code{\link{tstosscan}} function applies multiple hypothesis
	tests to several consecutive sub-portions of a time series of
	varying width. This function plots the results of such a
	set of multiple hypothesis tests.} 
\usage{
plottstosscan(x, inc, ttsobj, xlab = "Time", ylab = "x")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The time series under analysis.}
  \item{inc}{Inc is a number describing the start index of the series.
	E.g. if you analyzed a portion whose starting index value was,
	t=1234, then you would want the plot to have a horizontal axis
	whose initial label was somewhere near 1234, or wherever the
	time series started (not uniformly zero).}
  \item{ttsobj}{The object from \code{link{tstosscan}}
	containing the results of the multiple hypothesis test.}
  \item{xlab}{Supplied as the \code{xlab} argument of the plot}
  \item{ylab}{Supplied as the \code{ylab} argument of the plot}
}
\details{The plot first draws a light grey time series plot of the
	underlying time series. The left-hand axis quantifies the values.

	Then a series of ``horizontal lines'' is plotted, one for each
	span specified in the \code{spans} argument of the previous
	call to \code{\link{tstosscan}}. Each horizontal line is made
	up of a series of dashes or crosses. A dash indicates that
	the stationarity test applied to a subseries centred at that
	location, with the span (width) of time series values at the
	given span (given by the right hand axis), indicates no evidence
	for nonstationary (ie assume stationary here for now). A cross
	indicates that the series portion at that span, at that location
	is nonstationary.

	Overall, this plot can give you an idea of how nonstationary
	(second order) a time series is. A lot of crosses at small spans
	can indicate highly non-stationary series. A few crosses, even at
	large spans, indicate near-stationarity.

	It should be remembered that this plot produces a picture that
	summarizes the results of multiple hypothesis tests. This means
	that, e.g., for a size of test at 0.05 you would expect 1 in 20
	of the results to indicate non-stationarity, even if the underlying
	time series was completely stationary. 
}
\value{Nothing }
\references{`Costationary and stationarity tests for stock index returns' by Car
dinali and Nason, 2008, University of Bristol Technical Report 08:08.}
\author{Guy Nason}
\seealso{\code{\link{tstosscan}}}
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
