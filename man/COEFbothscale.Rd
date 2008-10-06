\name{COEFbothscale}
\alias{COEFbothscale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Produces plots from output of findstysol that attempt to
	group different solutions.}
\description{Uses hierarchical clustering and multidimensional scaling
	to produce a plot of all the convergence stationary
	solutions. These plots are designed to aid the user in
	identifying `unique' sets of stationary solutions.
}
\usage{
COEFbothscale(l, plotclustonly = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{l}{An object returned by \code{\link{findstysols}},
	which contains the results of an optimization
	to find solutions that correspond to stationary
	series which are the time-varying linear combination
	of two locally stationary time series.}
  \item{plotclustonly}{If \code{TRUE} then only produce the hierarchical
	clustering plot.}
  \item{\dots}{ Additional arguments to the hierarchical clustering plot.}
}
\details{The function \code{\link{findstysols}} runs multiple optimization
	runs (it can do only one is required) to try and find (multiple)
	solutions to the problem of finding costationary time series.
	It does this by running the optimization from multiple random
	starts. The solutions (if they exist) to these optimizations
	are not necessarily unique, and it is useful to find as many
	genuine different solutions as possible. However, even if there
	was one unique (mathematical) solution, due to different random
	starts, the vagaries of the optimizer, and the fact that the
	hypothesis test for stationarity is not a deterministic test,
	it is usually the case
	the many solutions very close to, but not identical to, the
	unique solutions might be found. However, they would morally
	be thought to be the same solution.

	In the case of multiple actual solutions, then the optimization
	algorithm might find many solutions close to the real ones and,
	of course, it might completely miss a real one (or not be near to
	a real one). However, the situation often arises where, after
	multiple starts, one ends up with clusters of computed solutions
	that group themselves around the `real' solution.

	In other words, the solution vectors can be viewed as a
	multivariate data set where the cases correspond to the results
	of different optimization runs and the variables correspond	
	to the coefficients of the time-varying linear combinations.

	Both multidimensional scaling (\code{cmdscale}) and
	hiearchical clustering (\code{hclust}) are used to determine
	possible clusterings of solutions. Then, representative members
	from these clusters can be further investigated with a function
	such as \code{\link{LCTSres}}
}
\value{The results of the multidimensional scaling and hierarchical
	clustering are returned as list with two components
	\code{epscale} and \code{epclust} respectively.  }
\references{`Costationary and stationarity tests for stock index returns' by Cardinali and Nason}
\author{Guy Nason}
\seealso{\code{\link{COEFscale}},\code{\link{findstysols}}, \code{\link{LCTSres}}}
\examples{
#
# To be run directly after the example in findstysols
#
\dontrun{COEFbothscale(tmp)}
}
\keyword{ts}
