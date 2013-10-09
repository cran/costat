\name{plot.csFSSgr}
\alias{plot.csFSSgr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Produce plots from a \code{csFSSgr} object.
}
\description{A \code{csFSS} object contains a set of solutions obtained
	from a series of optimizations. Each solution corresponds to
	a time-varying linear combination of two time series (or
	rather the wavelet coefficients of such combinations) where
	the combination has found to be stationary and the optimizer that
	got there converged. Often one wishes to interrogate the results,
	such as seeing how the solutions cluster, or what their low-dimensional
	scaling solution projection looks like, such analyses are produced
	by the \code{\link{COEFbothscale}} function and the whole
	plot is marshalled by the \code{\link{plot.csFSS}} function.
}
\usage{
\method{plot}{csFSSgr}(x, plotclust = TRUE, plotscale = TRUE, sollabels=FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The \code{csFSSgr} object to be plotted.
}
  \item{plotclust}{If TRUE then the dendrogram clustering is plotted,
	if FALSE it is not.
}
  \item{plotscale}{If TRUE then the scaling solution picture is plotted,
	if FALSE it is not.
}
  \item{sollabels}{If TRUE then solution numbers are plotted on the scaling
        plot, if produced.
}
  \item{\dots}{Other arguments to plot.
}
}
\value{None.
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

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{plot.csFSS}}
}
\examples{
#
# This function is a helper function for plot.csFSS so see the example there.
#
}
\keyword{ts}
