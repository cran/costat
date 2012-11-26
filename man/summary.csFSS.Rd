\name{summary.csFSS}
\alias{summary.csFSS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Summarize a \code{csFSS} object.
}
\description{Summarizes a \code{csFSS} object.
}
\usage{
\method{summary}{csFSS}(object, size = 0.05, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{Object you wish to summarize.
}
  \item{size}{A hypothesis test size. The \code{csFSS} object contains a
	number of p-values, this argument controls what is considered
	significant (but not corrected for multiple tests)
}
  \item{\dots}{Other arguments
}
}
\value{None
}
\references{
Cardinali, A. and Nason, G.P. (2012) Costationarity of Locally 
Stationary Time Series using \code{costat}.

Cardinali, A. and Nason, G.P. (2010) Costationarity of locally stationary
time series. \emph{J. Time Series Econometrics}, \bold{2}, Issue 2, Article 1.
}
\author{
Guy Nason
}


\seealso{\code{\link{findstysols}},\code{\link{plot.csFSS}}, \code{\link{print.csFSS}}
}
\examples{
#
# See example to print.csFSS which calls summary(.)
#
}
\keyword{ts}
