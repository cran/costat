\name{COEFscale}
\alias{COEFscale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphical aid for detecting clustering in optimization
	solutions.}
\description{Performs similar function to \code{\link{COEFbothscale}}
	but treats the two linear combination coefficients separately
	rather than in one go.
}
\usage{
COEFscale(l, plotclustonly = FALSE, plotalphaonly = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{l}{Results returned from \code{\link{findstysols}}, or
	merged solution sets from \code{\link{mergexy}}}
  \item{plotclustonly}{If TRUE then only the results of the
	hierarchical clustering are plotted.}
  \item{plotalphaonly}{ If TRUE then only the results of the
	first linear combination function are plotted.}
}
\details{Usually we recommend the use of \code{\link{COEFbothscale}}
	as the solution sets come in pairs and should be treated as
	a pair in analysis.}
\value{Nothing of interest
}
\references{`Costationary and stationarity tests for stock index returns' by Cardinali and Nason}
\author{Guy Nason}
\seealso{\code{\link{COEFbothscale}}, \code{\link{findstysols}}}
\examples{
#
# See example in COEFbothscale
#
}
\keyword{ts}
