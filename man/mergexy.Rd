\name{mergexy}
\alias{mergexy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Concatenate a set of solution results into one set}
\description{Merges several sets of optimization results from
	multiple calls to \code{\link{findstysols}} into 
	a single object for further analysis}
\usage{
mergexy(l)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{l}{An object containing a set of optimization solutions,
	such as that returned by \code{\link{findstysols}}} 
}
\details{
The return object from an invocation of the
	\code{\link{findstysols}} is a list containing a number
	of interesting components containing information about
	the starting parameters, the (hopefully optimal) ending
	parameters, convergence status, minimum variance achieved
	and p-value associated with the final test of stationarity
	after an optimization.

	It is possible to ask \code{\link{findstysols}} to execute
	multiple optimization runs in the same function, by choice of
	the \code{Nsims} parameter. However, for truly large runs,
	it can be convenient to run multiple copies of
	\code{\link{findstysols}}, for example on multiple processors
	simultaneously (a coarse grained parallelism).

	In particular, for large time series, it can be useful to run
	\code{\link{findstysols}} for \bold{one} optimization run
	(as running more than one for a very large series can cause the
	software to fail as R can run out of memory. Actually, for very
	very large series even one optmization run can fail for memory
	reasons). 

	In this way multiple optimization runs can be executed with each
	one producing its own set of results. This function
	(\code{mergexy}) takes a list of object names of all of the results,
	and merges the results into one object as if a single call
	to \code{\link{findstysols}} had been executed. Such a single
	set of results can then be passed on to further analysis
	routines, such as \code{\link{COEFbothscale}} or
	\code{\link{LCTSres}}.
}
\value{A set of optimization solutions in the same format as
	those returned by \code{\link{findstysols}}}
\references{`Costationary and stationarity tests for stock index returns' by Car
dinali and Nason, 2008, University of Bristol Technical Report 08:08.}
\author{Guy Nason}
\seealso{\code{\link{findstysols}}, \code{\link{LCTSres}},
	\code{\link{COEFbothscale}}}
\examples{
#
# Run two optimizations (don't run in R installation as it is compute intense)
#
\dontrun{solnset1 <- findstysols(Nsims=1, tsx=x2, tsy=y2)}
\dontrun{solnset2 <- findstysols(Nsims=1, tsx=x2, tsy=y2)}
#
# Merge them
#
\dontrun{solnset <- mergexy(solnset1, solnset2)}
#
# The previous three commands are equivalent to
#
\dontrun{solnset <- findstysols(Nsims=2, tsx=x2, tsy=y2)}
#
# Although the single command is more elegant the previous set of three
# has the advantage that the two calls to findstysols can be executed by,
# e.g. two different processors hence speeding overall execution time. 
#
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
