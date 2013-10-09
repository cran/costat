\name{plot.csBiFunction}
\alias{plot.csBiFunction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Plot a \code{csBiFunction} object }
\description{A \code{csBiFunction} object contains representations of
	two functions. This function plots the two functions superimposed.
}
\usage{
\method{plot}{csBiFunction}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of class \code{csBiFunction}
}
  \item{\dots}{Other arguments to plot call
}
}
\value{None
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

\seealso{\code{\link{coeftofn}}, \code{\link{print.csBiFunction}},
	\code{\link{summary.csBiFunction}}
}
\examples{
\dontrun{plot(coeftofn(c(1,-1), c(0.5, 0.5)))}
}
\keyword{ts}
