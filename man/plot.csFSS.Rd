\name{plot.csFSS}
\alias{plot.csFSS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a \code{csFSS} object.
}
\description{Produces two types of plot from the information in
	a \code{csFSS} object, such as that returned by
	\code{\link{findstysols}}.
}
\usage{
\method{plot}{csFSS}(x, solno = NULL, ALLplotclust = TRUE, ALLplotscale = TRUE, sollabels=TRUE,
    SNinc = 0, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The \code{csFSS} object you wish to produce plots for.
}
  \item{solno}{If missing then the plot produces plots that show information
	on all solutions at once, first in a scatter plot, then in a
	dendrogram. If provided then the plot produces information
	on that specific solution.
}
  \item{ALLplotclust}{If TRUE then the dendrogram is plotted, if FALSE
	it is not.
}
  \item{ALLplotscale}{If TRUE then the two-dimensional scaling solution
	is plotted. If FALSE, it is not.
}
  \item{sollabels}{If TRUE then solution numbers are plotted on the scaling
	plot, if produced.
}
  \item{SNinc}{An argument passed to the \code{\link{LCTSres}} function
	if called. When plotting add an increment on where to start
	looking at the time series/solutions from.
}
  \item{\dots}{Other arguments passed to plot.
}
}
\details{This function can produce either a scatterplot, which indicates
	the two-dimensional scaling picture of the optimization solution 
	sets, or a dendrogram showing putative clustering of solutions.
	In both cases it is a plot considering ALL solutions at once.
	These plots are delegated to the \code{\link{plot.csFSSgr}}
	function.

	If the argument \code{solno} is provided then plots are produced
	which show information on a single solution. This plot is
	delegated to the \code{\link{LCTSres}} function.
}
\value{None.
}
\references{
Cardinali, A. and Nason, G.P. (2012) Costationarity of Locally 
Stationary Time Series using \code{costat}.

Cardinali, A. and Nason, G.P. (2010) Costationarity of locally stationary
time series. \emph{J. Time Series Econometrics}, \bold{2}, Issue 2, Article 1.
}
\author{
G.P.Nason
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{findstysols}},
\code{\link{LCTSres}},
\code{\link{plot.csFSSgr}}, \code{\link{print.csFSS}},
\code{\link{summary.csFSS}}
}
\examples{
#
# Create dummy data
#
x1 <- rnorm(32)
y1 <- rnorm(32)
#
# Find stationary combinations
# Note: we don't run this example in installation/package formation as
# it takes a long time. However, this precise command IS run in
# the help to findstysols
#
\dontrun{ans <- findstysols(Nsims=100, tsx=x1, tsy=y1)}
#
# Produce dendrogram
#
\dontrun{plot(ans)}
#
# Produce four pictures relating to solution 3 (can also do
# par(mfrow=c(2,2)) to make a nice 4 plot on one page.)
#
\dontrun{plot(ans, solno=3)}
#solno is  3 
#3 
#1: 
}
\keyword{ts}
