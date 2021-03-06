\name{plotBS}
\alias{plotBS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Compute p-value for parametric Monte Carlo test and optionally
	plot test statistic values}
\description{Computes and returns a p-value for the result of a parametric
	Monte Carlo test. Optionally, plots a histogram of the test
	statistics (on the original data, and using test statistics resulting
	from simulations from the null hypothesis distribution). 
}
\usage{
plotBS(BS, alpha = 0.05, plot = TRUE, verbose = FALSE, main = "Bootstrap Histogram",
        xlab = "Test Statistic Values", ylab = "Frequency")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BS}{The results from a Monte Carlo test. This should be a vector
	of arbitrary length. The first value must be the value of the test
	statistic computed on the data. The remaining values are the
	test statistics computed on simulations constructed under the
	null hypothesis.}
  \item{alpha}{A nominal size for the test. This only effects the reporting.
	If the computed p-value is less than \code{alpha} then the function
	prints out that the series is not stationary.} 
  \item{plot}{If \code{TRUE} then a histogram of all the test statistics
	is produced, with a vertical line showing the position of the test
	statistic computed on the actual data. If the vertical line is much
	larger than all the histogram values then this is indicative of
	stationarity. If the vertical line is well within the histogram
	values then this is indicative of no evidence against stationarity.}
  \item{verbose}{If \code{TRUE} then the p-value is printed and a sentence
	declaring "stationary" or "not stationary" is printed (relative
	to the nominal p-value)}
  \item{main}{A \code{main} label for the plot, if produced}
  \item{xlab}{An \code{xlab} x axis label for the plot, if produced}
  \item{ylab}{An \code{ylab} y axis label for the plot, if produced}
}
\value{The p-value computed from the Monte Carlo test results is returned
}

\references{Cardinali, A. and Nason, Guy P. (2013) Costationarity of
	Locally Stationary Time Series Using costat.
	\emph{Journal of Statistical Software}, \bold{55}, Issue 1.

Cardinali, A. and Nason, G.P. (2010) Costationarity of locally stationary
time series. \emph{J. Time Series Econometrics}, \bold{2}, Issue 2, Article 1.
}
\author{Guy Nason}
\seealso{\code{\link{getpvals}},\code{\link{BootTOS}}}
\examples{
#
# See example in \code{\link{BootTOS}}.
#
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts} 
