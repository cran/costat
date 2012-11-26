\name{plot.BootTOS}
\alias{plot.BootTOS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plots results of a Bootstrap Test of Stationarity 
}
\description{Produces Bootstrap simulation result as a histogram
	with a vertical line indicating the test statistic computed
	on the actual data. 
}
\usage{
\method{plot}{BootTOS}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The object you wish to get a plot on.
}
  \item{\dots}{Other arguments to plot.
}
}
\details{Produces a histogram of all the bootstrap statistics
	and the test statistic computed on the true data.
	Also produces a vertical line indicating the position
        of the true statistic.
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
G.P. Nason
}

\seealso{\code{\link{BootTOS}}}
\examples{
#
v <- rnorm(512)
\dontrun{v.BootTOS <- BootTOS(v)}
\dontrun{plot(v.BootTOS)}
}
\keyword{ts}
