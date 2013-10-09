\name{print.csBiFunction}
\alias{print.csBiFunction}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Print a \code{csBiFunction} object.  }
\description{
A \code{csBiFunction} object contains representations of
        two functions. This function prints information about the object 
}
\usage{
\method{print}{csBiFunction}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The object you want printed.
}
  \item{\dots}{Other arguments }
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
Guy Nason
}

\seealso{\code{\link{plot.csBiFunction}}, \code{\link{summary.csBiFunction}}
}
\examples{
print(coeftofn(c(1,-1), c(0.5, 0.5)))
#Class 'csBiFunction' : Contains two sampled functions:
#       ~~~~  : List with 2 components with names
#              alpha beta 
#
#
#summary(.):
#----------
#Length of functions is:  256 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ts}
