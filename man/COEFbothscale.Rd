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
COEFbothscale(l, plotclustonly = FALSE, StyPval=0.05, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{l}{An object returned by \code{\link{findstysols}},
	of class \code{csFSS},
	which contains the results of an optimization
	to find solutions that correspond to stationary
	series which are the time-varying linear combination
	of two locally stationary time series.}
  \item{plotclustonly}{If \code{TRUE} then only produce the hierarchical
	clustering plot.}
  \item{StyPval}{The p-value by which solutions are deemed to be
	stationary or not for inclusion into plots. If the p-value
	for a particular solution is greater than \code{StyPval} then
	the solution is deemed stationary and included.}
  \item{\dots}{ Additional arguments to the hierarchical clustering plot.}
}
\details{The function \code{\link{findstysols}} uses numerical
	optimization to try and discover time-varying linear combinations
	of two time series to find a combination which is stationary.
	Like many numerical optimizations the optimizer is supplied with
	starting coordinates and proceeds through an optimization
	routine to end coordinates which are located at the minimum
	(in this case). So, the user has a choice over where to start
	each optimization.

	A priori there is no recipe for knowing where to start the
	optimizer, so such situations are usually handled by running
	the optimizer many time each time starting in a different position.
	The solution here is to start from a set of different randomly
	chosen starting points. After the optimizer is run from these
	different starting positions it ends up in the same number of
	potentially different ending positions.

	However, some of the ending solutions might be identical,
	some might be very close, some might be reflections (e.g.
	the if the coefficients (a,b) result in a stationary solution then
	so does (-a, -b)). Morally, though, all of these cases would
	reference the same solution.

	Hence, we require some method for identifying the set of unique
	solutions. We can be considerably aided in this task by
	multidimensional scaling (which uses inter-solution distances
	to produce a map of how close solution sets really are)
	or hierarchical clustering (which can produce a nice picture
	to indicate how the solutions might be related).

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
\value{An object of class \code{csFSSgr} is returned containing the
	following components: the results of the multidimensional scaling
	and hierarchical
	clustering are returned as list with two components
	\code{epscale} and \code{epclust} respectively, and
	the input \code{l} object is returned as component \code{x}
	and the \code{StyPval} object is returned as a component.  }
\references{
Cardinali, A. and Nason, G.P. (2012) Costationarity of Locally 
Stationary Time Series using \code{costat}.

Cardinali, A. and Nason, G.P. (2010) Costationarity of locally stationary
time series. \emph{J. Time Series Econometrics}, \bold{2}, Issue 2, Article 1.
}
\author{Guy Nason}
\seealso{\code{\link{findstysols}}, \code{\link{LCTSres}}}
\examples{
#
# See example in findstysols
#
}
\keyword{ts}
